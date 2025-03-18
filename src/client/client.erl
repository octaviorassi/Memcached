-module(client).
-export([start/1, put/2, del/1, get/1, stats/0, startDefault/0, quit/0, client/1]).
-include("protocol.hrl").

-define(BUCKET_FACTOR, 10).
-record(serverMap, {size, servers, identifier, available_sockets}).
-type server_map() :: #serverMap{ size :: non_neg_integer(),
                                  servers :: [gen_tcp:socket()],
                                  identifier :: atom(),
                                  available_sockets :: [gen_tcp:socket()]}.

-record(request, {command, key, value, pid}).
-type request() :: #request{ command :: atom(),
                           key :: term(),
                           value :: term(),
                           pid :: pid() }.
-define(PRINT(Value), io:format("[Debug] ~p~n", [Value])).



%% @doc Determina si el alias para un PID ya ha sido registrado previamente.
%% @param PidAlias el alias cuyo registro queremos chequear.
%% @return no si no estaba registrado, si en caso contrario.
-spec is_registered(atom()) -> no | yes.
is_registered(PidAlias) ->
  case whereis(PidAlias) of
    undefined -> no;
    _         -> yes
  end.



%% @doc Finaliza la ejecucion del cliente y des-registra al alias cliente.
%% @return notExistingClient si no se habia lanzado un cliente aun, o clientClosed si se logra efectivamente finalizar la conexion.
-spec quit() -> notExistingClient | clientClosed.
quit() ->

  case is_registered(client) of 
    
    no -> notExistingClient;  
    
    yes -> client ! quitClient,
           unregister(client),
           clientClosed
  end.
  

%% @doc Toma una lista de pares IP y puerto correspondientes a servidores memcached, crea el socket para la conexion con cada uno de ellos, y genera el proceso cliente que se utilizara para hacer los pedidos. Registra tambien el PID del cliente como ClientPID.
%% @param ServerList una lista de tuplas {IP, puerto} cada una correspondiente a un servidor memcached.
%% @return existingClient si ya se habia registrado a un cliente previamente, clientCreated si no se habia registrado previamente y se logro lanzar el proceso cliente exitosamente.
-spec start([{string(), non_neg_integer()}]) -> existingClient | clientCreated.
start(ServerList) ->

  Length = length(ServerList),

  case is_registered(client) of 
    yes -> existingClient;
    no  -> case utils:create_sockets(ServerList) of
    
            {createSocketsError, ServerInfo} -> {invalidServer, ServerInfo};

            Sockets -> Servers = [lists:nth((I rem Length) + 1, Sockets) || I <- lists:seq(0, Length * ?BUCKET_FACTOR - 1)],
                       ServerMap = #serverMap{size = Length, servers = Servers, available_sockets = Sockets},
                       ClientPID = spawn(?MODULE, client, [ServerMap]),
                       register(client, ClientPID),
                       clientCreated
          end
  end.
  
  
%% @doc Inicializa el proceso cliente con valores establecidos por defecto. Considera funcionando a dos servidores en la direccion de loopback de IPv4 en los puertos 8000 y 9000.
%% @return existingClient si ya se habia registrado a un cliente previamente, clientCreated si no se habia registrado previamente y se logro lanzar el proceso cliente exitosamente.
-spec startDefault() -> existingClient | clientCreated.
startDefault() ->
  start([{"127.0.0.1", 8000}, {"127.0.0.1", 9000}]).


%% @doc Almacena el par clave-valor indicado en el servidor memcached asociado a la instancia del cliente en ejecucion. 
%% @param Key La clave a almacenar.
%% @param Value El valor asociado a la clave.
%% @return La respuesta del cliente, que puede ser un OK o un mensaje de error indicando la causa de fracaso de la operacion.
-spec put(term(), term()) -> term().
put(Key, Value) ->

  client ! { put, Key, Value, self() }, 

  receive 
    Response -> Response
  end.


%% @doc Elimina el par clave-valor asociado a la clave objetivo en la instancia de memcached en ejecucion.
%% @param Key La clave del par a eliminar.
%% @return La respuesta del cliente, que puede ser un OK o un mensaje de error indicando la causa de fracaso de la operacion.
-spec del(term()) -> term().
del(Key) -> 

  client ! { del, Key, self() },
  
  receive 
    Response -> Response
  end.


%% @doc Obtiene el valor asociado a la clave objetivo en la instancia de memcached en ejecucion.
%% @param Key La clave del par a buscar.
%% @return La respuesta del cliente, que puede ser un OK o un mensaje de error indicando la causa de fracaso de la operacion.
-spec get(term()) -> term().
get(Key) ->
  client ! { get, Key, self() },
  
  receive 
    Response -> Response
  end.

% todo
stats() ->
  client ! { stats, self() },
  
  receive 
    Response -> Response
  end.


%% @doc Dada una lista de sockets, un socket objetivo, y un listado de sockets de servidores disponibles, reemplaza cada aparicion del socket objetivo en la lista por un socket escogido aleatoriamente del listado de sockets disponibles.
%% @param Sockets Lista de sockets sobre la que se hara el reemplazo.
%% @param ServerSocket El servidor a reemplazar en cada aparicion.
%% @param AvailableSockets Lista de sockets disponibles para el reemplazo.
%% @return Una lista donde cada aparicion del socket objetivo fue reemplazada por un socket aleatorio del listado de sockets disponibles.
-spec replace_sockets([gen_tcp:socket()], gen_tcp:socket(), [gen_tcp:socket()]) -> [gen_tcp:socket()].
replace_sockets([], _, _) -> [];
replace_sockets([Socket | Sockets], ServerSocket, AvailableSockets) ->
  ReplaceSockets = replace_sockets(Sockets, ServerSocket, AvailableSockets), 
  case Socket == ServerSocket of
    
    true  ->
      Index = rand:uniform(length(AvailableSockets)),
      ReplacementSocket = lists:nth(Index, AvailableSockets),
      [ReplacementSocket | ReplaceSockets];
    
    false -> 
      [Socket | ReplaceSockets]
  end.


%% @doc Dado un ServerMap con la informacion de los servidores corriendo y un socket objetivo, reemplaza cada aparicion del socket objetivo en el ServerMap por un socket elegido aleatoriamente de entre los demas sockets disponibles, rebalanceando asi la carga entre todos los servidores.
%% @param ServerMap La estructura con la informacion de los servidores memcached en ejecucion.
%% @param ServerSocket El socket del servidor a reemplaar.
%% @return Un nuevo ServerMap con el rebalanceo de servidores aplicado.
-spec rebalance_servers(server_map(), gen_tcp:socket()) -> server_map().
rebalance_servers(ServerMap, ServerSocket) ->
  
  Id      = ServerMap#serverMap.identifier, 
  Size    = ServerMap#serverMap.size, 
  Servers = ServerMap#serverMap.servers, 
  Sockets = ServerMap#serverMap.available_sockets, 

  AvailableSockets = lists:delete(ServerSocket, Sockets),
  RebalancedServers = replace_sockets(Servers, ServerSocket, AvailableSockets),

  NewServerMap = #serverMap{ size = Size, 
                             servers = RebalancedServers,
                             available_sockets = AvailableSockets,
                             identifier = Id},
  NewServerMap.


%% @doc Dada una clave y la estructura con la informacion de los servidores, obtiene el bucket (servidor) asociado a la clave a traves de la funcion hash.
%% @param Key La clave para la cual queremos determinar el servidor.
%% @param ServerMap La estructura con la informacion de los servidores.
%% @return El socket del servidor asociado a la clave. 
-spec get_server(term(), server_map()) -> gen_tcp:socket().
get_server(Key, ServerMap) ->

    BinaryHash = crypto:hash(sha, Key),

    MapIndex = binary:decode_unsigned(BinaryHash),
    Index = (MapIndex rem (ServerMap#serverMap.size)) + 1,

    lists:nth(Index, ServerMap#serverMap.servers).


%% @doc Cierra la conexion con cada uno de los sockets de la lista de sockets objetivo.
%% @param ServerSockets La lista de sockets con los cuales queremos terminar la conexion.
%% @return El atom ok.
-spec close_server_sockets([gen_tcp:socket()]) -> ok.
close_server_sockets(ServerSockets) ->
  lists:foreach(fun gen_tcp:close/1, ServerSockets).


%% todo @doc Dado un socket y un atomo representando una operacion, ...
handle_ok(ServerSocket, get) ->
  <<ValueLength:32/big>> = utils:recv_bytes(ServerSocket, 4),
  BinaryValue = utils:recv_bytes(ServerSocket, ValueLength),
  Value = binary_to_term(BinaryValue),
  {ok, Value};
handle_ok(_, stats) -> todo;
handle_ok(_, _) -> ok. 


%% @doc Dado un request genera la tupla cuyos elementos son los campos del record request en el mismo orden.
%% @param Request El request a desarmar.
%% @return La tupla con los campos del request objetivo.
-spec request_to_tuple(request()) -> {atom(), term(), term(), pid()}.
request_to_tuple(Request) ->
  { Request#request.command,
    Request#request.key,
    Request#request.value,
    Request#request.pid }.


%% todo @doc Dado un request.. (todo, no entendi).
handle_server_error_response(Request) ->
  case Request#request.command of 
    put -> request_to_tuple(Request);
    _   -> enotfound
  end.

%% todo @doc Dado un request.. (todo, no entendi).
handle_server_error_target(Request) ->
  case Request#request.command of 
    put -> client;
    _   -> Request#request.pid
  end.


%! REVISAR ESTA DOCUMENTACION.
%% @doc Dado el socket de un servidor, la estructura con la informacion de los servidores, y un request, lee la respuesta del servidor al request. Si es un error, suponemos que el servidor ha caido y se rebalancea los servers. En todos los casos, dependiendo de la respuesta del servidor, se retorna el mensaje de error correspondiente o, si se recibio un OK, se lee la respuesta correspondiente y se la retorna junto con su objetivo.
%% @param ServerSocket El socket del servidor del cual esperamos respuesta.
%% @param ServerMap La estructura con informacion de los servidores corriendo.
%% @param Request El request para el cual esperamos respuesta.
%% @return Una tupla con la respuesta, la estructura con la informacion de los servidores actualizada, y el target de la respuesta.
-spec handle_response(gen_tcp:socket(), server_map(), request()) -> {term(), server_map(), pid()}.
handle_response(ServerSocket, ServerMap, Request) ->

  Command = utils:recv_bytes(ServerSocket, 1),

  NewServerMap = 
    case Command of 
      serverError -> rebalance_servers(ServerMap, ServerSocket); % Actualizamos los servers
      _           -> ServerMap                                   % Mantenemos los mismos
    end,

  Response = 
    case Command of 
      serverError    -> handle_server_error_response(Request);
      <<?OK>>        -> handle_ok(ServerSocket, Request#request.command);
      <<?EBIG>>      -> ebig;
      <<?ENOTFOUND>> -> enotfound
    end,

  Target = 
    case Command of 
      serverError -> element(1, handle_server_error_target(Request));
      % aca obtiene el primer elemento de la tupla handle_server_error_target(Request),
      % pero esa expresion nunca evalua a una tupla, evalua a un atom o a un pid, entonces explota porque element no tipa
      _           -> Request#request.pid
    end,

  {Response, NewServerMap, Target}.

%! REVISAR EL RETURN DE ESTO Y COMO EXPLICARLO MEJOR.
%% @doc Dada la estructura con la informacion de los servidores memcached corriendo, recibe los pedidos del cliente y los responde indefinidamente.
%% @param ServerMap La estructura con informacion de los servidores memcached corriendo.
-spec client(server_map()) -> term().
client(ServerMap) ->

  receive

    { put, Key, Value, PID } -> 

      Request = #request{command = put, key = Key, value = Value, pid = PID},

      {BinaryKey, KeyLength}     = utils:binary_convert(Key),
      {BinaryValue, ValueLength} = utils:binary_convert(Value),

      ServerSocket = get_server(BinaryKey, ServerMap),

      PutMessage = utils:create_message(?PUT, KeyLength, BinaryKey, ValueLength, BinaryValue),

      gen_tcp:send(ServerSocket, PutMessage), 

      {Response, NewServerMap, Target} = handle_response(ServerSocket, ServerMap, Request),

      Target ! Response, 

      client(NewServerMap);

    { del, Key, PID } ->

      Request = #request{command = del, key = Key, pid = PID},

      { BinaryKey, KeyLength} = utils:binary_convert(Key),
      ServerSocket = get_server(BinaryKey, ServerMap),

      DelMessage = utils:create_message(?DEL, KeyLength, BinaryKey),

      gen_tcp:send(ServerSocket, DelMessage),

      {Response, NewServerMap, Target} = handle_response(ServerSocket, ServerMap, Request),

      Target ! Response, 

      client(NewServerMap);

    { get, Key, PID } ->
      
      Request = #request{command = get, key = Key, pid = PID},

      {BinaryKey, KeyLength} = utils:binary_convert(Key),
      ServerSocket = get_server(BinaryKey, ServerMap),

      GetMessage = utils:create_message(?GET, KeyLength, BinaryKey),

      gen_tcp:send(ServerSocket, GetMessage),

      {Response, NewServerMap, Target} = handle_response(ServerSocket, ServerMap, Request),

      Target ! Response, 

      client(NewServerMap);

    { stats, PID } -> PID;


    quitClient -> close_server_sockets(ServerMap#serverMap.available_sockets);

    Other -> 
      error(error),
      io:fwrite("[Error] Invalid request: ~p~n",[Other])
  end.



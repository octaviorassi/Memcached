-module(client).
-export([start/1, put/2, del/1, get/1, stats/0, status/0, startDefault/0, quit/0, client/1]).
-include("protocol.hrl").

-define(BUCKET_FACTOR, 10).

-record(serverInfo, {socket, id, putCounter}).
-type server_info() :: #serverInfo { socket :: gen_tcp:socket(),
                                     id :: non_neg_integer(),
                                     putCounter :: non_neg_integer() }.


-record(serversTable, {size, initial_num_server, servers, identifier, servers_info}).
-type servers_table() :: #serversTable{ size :: non_neg_integer(),
                                        initial_num_server :: non_neg_integer(),
                                        servers :: [gen_tcp:socket()],
                                        identifier :: atom(),
                                        servers_info :: [server_info()] }.

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
  

create_servers_info(Sockets) ->
  [#serverInfo{socket = Socket, id = Id, putCounter = 0} || {Socket, Id} <- lists:zip(Sockets, lists:seq(1, length(Sockets)))].


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
                       ServersInfo = create_servers_info(Sockets),
                       ServersTable = #serversTable{size = Length, initial_num_server = Length, servers = Servers, servers_info = ServersInfo},
                       ClientPID = spawn(?MODULE, client, [ServersTable]),
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

%% @doc Obtiene las estadisticas 
%% @return La respuesta del cliente, que 
status() -> % Status del cliente, cuantos puts hizo a cada server
  client ! { status, self() },
  
  receive 
    Response -> io:fwrite(Response)
  end.

%% @doc Obtiene las estadisticas de los servidores a los que el cliente se encuentra conectado.
%% @return La respuesta del cliente, que nos muestra un mensaje
stats() ->
  client ! {stats, self() },

  receive 
    Response -> io:fwrite(Response) 
  end.







%% @doc Dada una lista de sockets, un socket objetivo, y una lista de la informacion de los servidores disponibles, reemplaza cada aparicion del socket objetivo en la lista por un socket de uno de los servers disponibles, elegido de manera aleatoria.
%% @param Sockets Lista de sockets sobre la que se hara el reemplazo.
%% @param ServerSocket El servidor a reemplazar en cada aparicion.
%% @param ServersInfo Lista con informacion de todos los servidores disponibles.
%% @return Una lista donde cada aparicion del socket objetivo fue reemplazada por un socket aleatorio de los servers disponibles.
-spec replace_sockets([gen_tcp:socket()], gen_tcp:socket(), [server_info()]) -> [gen_tcp:socket()].
replace_sockets([], _, _) -> [];
replace_sockets([Socket | Sockets], ServerSocket, ServersInfo) ->
  ReplaceSockets = replace_sockets(Sockets, ServerSocket, ServersInfo), % Reemplazamos recursivamente
  case Socket == ServerSocket of
    
    true  ->
      Index = rand:uniform(length(ServersInfo)),
      Replacement = lists:nth(Index, ServersInfo),
      ReplacementSocket = Replacement#serverInfo.socket, 
      [ReplacementSocket | ReplaceSockets];
    
    false -> 
      [Socket | ReplaceSockets]
  end.







%% @doc Dado un ServersTable con la informacion de los servidores corriendo y la informacion de un servidor objetivo, reemplaza cada aparicion del socket del server objetivo en el ServersTable por un socket elegido aleatoriamente de entre los demas sockets de los servers disponibles, rebalanceando asi la carga entre todos los servidores.
%% @param ServersTable La estructura con la informacion de los servidores memcached en ejecucion.
%% @param ServerInfo La informacion del server que queremos reemplazar.
%% @return Un nuevo ServersTable con el rebalanceo de servidores aplicado.
-spec rebalance_servers(servers_table(), server_info()) -> servers_table().
rebalance_servers(ServersTable, ServerInfo) ->
  
  Id               = ServersTable#serversTable.identifier, 
  Size             = ServersTable#serversTable.size, 
  InitialNumServer = ServersTable#serversTable.initial_num_server,
  Servers          = ServersTable#serversTable.servers, 
  ServersInfo      = ServersTable#serversTable.servers_info, 

  NewServersInfo = lists:delete(ServerInfo, ServersInfo), % Elimino la informacion del servidor 
  RebalancedServers   = replace_sockets(Servers, ServerInfo#serverInfo.socket, NewServersInfo), % Rebalanceo la carga

  % Creo la nueva tabla de servidores
  NewServersTable = #serversTable{ size = Size,
                                   initial_num_server = InitialNumServer, 
                                   servers = RebalancedServers,
                                   servers_info = NewServersInfo,
                                   identifier = Id },
  NewServersTable.







%% @doc Dada una clave y un ServersTable con la informacion de los servidores, obtiene el bucket (servidor) asociado a la clave a traves de la funcion hash.
%% @param Key La clave para la cual queremos determinar el servidor.
%% @param ServersTable La estructura con la informacion de los servidores.
%% @return El socket del servidor asociado a la clave. 
-spec get_server(term(), servers_table()) -> gen_tcp:socket().
get_server(Key, ServersTable) ->

    BinaryHash = crypto:hash(sha, Key),

    MapIndex = binary:decode_unsigned(BinaryHash),
    Index = (MapIndex rem (ServersTable#serversTable.size)) + 1,

    lists:nth(Index, ServersTable#serversTable.servers).






%% @doc Cierra la conexion con cada uno de los sockets de la lista de sockets objetivo.
%% @param ServersInfo La lista con la informacion de los servidores con los cuales queremos terminar la conexion.
%% @return El atom ok.
-spec close_server_sockets([server_info()]) -> ok.
close_server_sockets(ServersInfo) ->
  lists:foreach(fun({ServerSocket,_,_}) -> gen_tcp:close(ServerSocket) end, ServersInfo).









%% @doc Dado un socket y un atomo representando una operacion, 
%% @param ServerSocket
%% @param Operation Es un atomo que representa que tipo de operacion se realizo contra el servidor.
%% @return 
-spec handle_ok(gen_tcp:socket(), atom()) -> atom().
% Si hicimos una operacion de GET, tenemos que recibir el resto del mensaje
handle_ok(ServerSocket, get) -> 
  <<ValueLength:32/big>> = utils:recv_bytes(ServerSocket, 4),
  BinaryValue = utils:recv_bytes(ServerSocket, ValueLength),
  Value = binary_to_term(BinaryValue),
  {ok, Value};
% Si hicimos operacion de DEL o PUT, unicamente devolvemos ok
handle_ok(_, _) -> io:fwrite("LLEGUE~n"), ok. 








%% @doc Dado un request genera la tupla cuyos elementos son los campos del record request en el mismo orden.
%% @param Request El request a desarmar.
%% @return La tupla con los campos del request objetivo.
-spec request_to_tuple(request()) -> {atom(), term(), term(), pid()}.
request_to_tuple(Request) ->
  { Request#request.command,
    Request#request.key,
    Request#request.value,
    Request#request.pid }.






%% @doc Se encarga de generar un termino de respuesta apropiado cuando una request tiene un error. En el caso de que estemos haciendo una operacion PUT, el mensaje tiene que ser la misma request, ya que intentaremos realizar nuevamente la operacion de PUT. En cualquier otro caso (GET, DEL) devolvemos enotfound.
%% @param Request El request para el cual se genero un error del servidor.
%% @return El termino adecuado dependiendo del tipo de request que estemos tratando.
-spec handle_server_error_response(request()) -> {atom(), term(), term(), pid()} | enotfound. 
handle_server_error_response(Request) ->
  case Request#request.command of 
    put   -> request_to_tuple(Request);  
    _   -> enotfound                  
  end.





%% @doc Se encarga de decidir el objetivo a responder cuando una request tiene un error. En el caso de que estemos tratando con una operacion PUT
%% @param Request El request para el cual se genero un error del servidor.
%% @return El objetivo a responder dependiendo del tipo de request que estemos tratando.
-spec handle_server_error_target(request()) -> pid() | client.
handle_server_error_target(Request) ->
  case Request#request.command of 
    put -> client;
    _   -> Request#request.pid
  end.


%! REVISAR ESTA DOCUMENTACION.
%% @doc Dado el socket de un servidor, la estructura con la informacion de los servidores, y un request, lee la respuesta del servidor al request. Si es un error, suponemos que el servidor ha caido y se rebalancea los servers. En todos los casos, dependiendo de la respuesta del servidor, se retorna el mensaje de error correspondiente o, si se recibio un OK, se lee la respuesta correspondiente y se la retorna junto con su objetivo.
%% @param ServerSocket El socket del servidor del cual esperamos respuesta.
%% @param ServersTable La estructura con informacion de los servidores corriendo.
%% @param Request El request para el cual esperamos respuesta.
%% @return Una tupla con la respuesta, la estructura con la informacion de los servidores actualizada, y el target de la respuesta.
-spec handle_response(gen_tcp:socket(), servers_table(), request()) -> {term(), servers_table(), pid()}.
handle_response(ServerSocket, ServersTable, Request) ->

  ResponseCode = utils:recv_bytes(ServerSocket, 1),
  
  NewServersTable = 
    case ResponseCode of 
      serverError -> rebalance_servers(ServersTable, ServerSocket); % Hubo un error, actualizamos los servers rebalanceando la carga
      _           -> ServersTable                                   % Mantenemos los mismos servidores
    end,

  Response = 
    case ResponseCode of 
      serverError    -> handle_server_error_response(Request);             % Manejamos la respuesta del error dependiendo del tipo pedido que se hizo
      <<?OK>>        -> handle_ok(ServerSocket, Request#request.command);  % Seguimios recibiendo luego del OK dependiendo del pedido que hicimos
      <<?ENOTFOUND>> -> enotfound;
      <<?EBIG>>      -> ebig                                               
    end,


  Target = 
    case ResponseCode of 
      serverError -> handle_server_error_target(Request); % Si hubo error, dependiendo el tipo del pedido, le respondemos al que nos hizo el pedido o 
                                                          % se reenvia nuevamente al cliente para reintentar la operacion.

      _           -> Request#request.pid                  % Si no hubo error, le respondemos al que nos hizo el pedido
    end,

  {Response, NewServersTable, Target}.


%% @doc Dada la estructura con la informacion de los servidores memcached corriendo, recibe los pedidos del cliente y los responde indefinidamente, o hasta que se haga un pedido de cierre.
%% @param ServersTable La estructura con informacion de los servidores memcached corriendo.
-spec client(servers_table()) -> term().
%!! TERMINAR DE HACER CLIENTE, NO GENERALIZAR EL RECIBO DE MENSAJE PARA NO COMPLEJIZARLO
client(ServersTable) ->

  receive

    { put, Key, Value, PID } -> 

      Request = #request{command = put, key = Key, value = Value, pid = PID},

      {BinaryKey, KeyLength}     = utils:binary_convert(Key),
      {BinaryValue, ValueLength} = utils:binary_convert(Value),

      ServerSocket = get_server(BinaryKey, ServersTable),

      PutMessage = utils:create_message(?PUT, KeyLength, BinaryKey, ValueLength, BinaryValue),
      gen_tcp:send(ServerSocket, PutMessage),

      % Si yo hice un put, se que solamente puedo recibir serverError, OK,  EBIG

      ResponseCode = utils:recv_bytes(ServerSocket, 1),

      Response = 
        case ResponseCode of
          serverError -> serverError;
          <<?OK>>     -> ok;
          <<?EBIG>>   -> ebig
        end,

      NewServersTable = 
        case ResponseCode of
          serverError -> rebalance_servers(ServerSocket, ServersTable);
          _           -> ServersTable
        end,

      Target = 
        case ResponseCode of
          serverError -> 

      {Response, NewServersTable, Target} = 
        case ResponseCode of  
          serverError -> {serverError, rebalance_servers()}; % Hay que hacer un handler de error
          <<?OK>>     -> handle_ok(ServerSocket, Request#request.command);  % Seguimios recibiendo luego del OK dependiendo del pedido que hicimos
          <<?EBIG>>   -> ebig                                               
        end,

      Target ! Response, 

      client(NewServersTable);


      NewServersTable = 
        case ResponseCode of
          serverError -> rebalance_servers(ServersTable, ServerSocket);
          _           -> ServersTable
        end,

      Response = 
        case ResponseCode of 
          serverError    -> handle_server_error_response(Request);             % Manejamos la respuesta del error dependiendo del tipo pedido que se hizo
          <<?OK>>        -> handle_ok(ServerSocket, Request#request.command);  % Seguimios recibiendo luego del OK dependiendo del pedido que hicimos
          <<?EBIG>>      -> ebig                                               
        end,



      {Response, NewServersTable, Target} = handle_response(ServerSocket, ServersTable, Request),

      


    { del, Key, PID } ->

      Request = #request{command = del, key = Key, pid = PID},

      { BinaryKey, KeyLength} = utils:binary_convert(Key),
      ServerSocket = get_server(BinaryKey, ServersTable),

      DelMessage = utils:create_message(?DEL, KeyLength, BinaryKey),
 
      gen_tcp:send(ServerSocket, DelMessage),

      % Si yo hago un DEL, puedo recibir OK, ENOTFFOUND, serverError
      {Response, NewServersTable, Target} = handle_response(ServerSocket, ServersTable, Request),

      Target ! Response, 
      client(NewServersTable);

    { get, Key, PID } ->
      
      Request = #request{command = get, key = Key, pid = PID},

      {BinaryKey, KeyLength} = utils:binary_convert(Key),
      ServerSocket = get_server(BinaryKey, ServersTable),

      GetMessage = utils:create_message(?GET, KeyLength, BinaryKey),


      gen_tcp:send(ServerSocket, GetMessage),

      % Si yo hago un DEL, puedo recibir OK v, ENOTFFOUND, serverError

      {Response, NewServersTable, Target} = handle_response(ServerSocket, ServersTable, Request),

      Target ! Response, 
      client(NewServersTable);

    { stats, PID } -> 
      StatsMessage = create_stats_message(ServersTable#serversTable.servers_info, ServersTable#serversTable.initial_num_server),
      PID ! StatsMessage,
      client(ServersTable); % Nos tendriamos que fijar si se cayo alguno de los servers durante la creacion de stats 

    { status, PID} -> 
      PutCounters = lists:map(fun(ServerInfo) -> ServerInfo#serverInfo.putCounter end, ServersTable#serversTable.servers_info),
      TotalPuts = lists:sum(PutCounters),

      StatusMessage = create_status_message(TotalPuts, ServersTable#serversTable.servers_info, ServersTable#serversTable.initial_num_server), 

      PID ! StatusMessage,
      client(ServersTable);

    quitClient -> close_server_sockets(ServersTable#serversTable.servers_info);


    Other -> 
      error(error),
      io:fwrite("[Error] Invalid request: ~p~n",[Other])

  end.


stats_requests(Id, ServersInfo) -> 
  case lists:keyfind(Id, 2, ServersInfo) of 
    
    {Socket, _, _} -> 
      StatsMessage = utils:create_message(?STATS),
      gen_tcp:send(Socket, StatsMessage),
      Response = utils:recv_bytes(Socket, 1),

      case Response of
        serverError -> io_lib:format("Server ~p: shutdown", [Id]);
        <<?OK>>     -> 
          <<ValueLength:32/big>> = utils:recv_bytes(Socket, 4),
          BinaryValue = utils:recv_bytes(Socket, ValueLength),
          String = binary_to_list(BinaryValue),
          io_lib:format("Server ~p: ~p", [Id, String])
      end;

    false          -> io_lib:format("Server ~p: shutdown", [Id])
  end.



create_stats_message(ServersInfo, InitialNumServer) ->
  IndexList = lists:seq(1, InitialNumServer),
  ServersStatsMessage = lists:map(fun(Id) -> stats_requests(Id, ServersInfo) end, IndexList),
  StatsMessage = string:join(ServersStatsMessage, "~n"),
  StatsMessage.

% TODO: no se estan actualizando los puts
% TODO: Tendriamos que fijarnos si total puts no es cero, ahi devolvemos que no se hicieron puts todavia
create_status_message(TotalPuts, ServersInfo, InitialNumServer) ->
  IndexList = lists:seq(1, InitialNumServer),
  ServersInfoTuple = lists:map(fun(S) -> { S#serverInfo.id, S#serverInfo.putCounter } end, ServersInfo),
  ServerCounterMessage = 
    lists:map(fun(Id) ->
      case lists:keyfind(Id, 1, ServersInfoTuple) of
         {_, PutCounter} -> io_lib:format("Server ~p: ~p% Keys", [Id, PutCounter/TotalPuts * 100]);
        false              -> io_lib:format("Server ~p: shutdown", [Id])
      end
    end, IndexList),

  StatusMessage = string:join(ServerCounterMessage, "~n"),
  StatusMessage.
-module(client).
-export([start/1, put/2, del/1, get/1, stats/0, status/0, startDefault/0, quit/0, client/1]).
-include("protocol.hrl").

-define(BUCKET_FACTOR, 10).

-record(serverInfo, {socket, id, keyCounter, address}).
-type server_info() :: #serverInfo { socket     :: gen_tcp:socket(),
                                     id         :: non_neg_integer(),
                                     keyCounter :: non_neg_integer(),
                                     address    :: {string(), non_neg_integer()} }.


-record(serversTable, {size, initial_num_server, servers, identifier, servers_info}).
-type servers_table() :: #serversTable{ size :: non_neg_integer(),
                                        initial_num_server :: non_neg_integer(),
                                        servers :: [gen_tcp:socket()],
                                        identifier :: atom(), %!! IDENTIFICADOR: completar
                                        servers_info :: [server_info()] }.


-define(PRINT(Value), io:format("[Debug] ~p~n", [Value])).


%% @doc Toma una lista de pares IP y puerto correspondientes a servidores memcached, crea el socket para la conexion con cada uno de ellos, y genera el proceso cliente que se utilizara para hacer los pedidos. Registra tambien el PID del cliente como ClientPID.
%% @param ServerList una lista de tuplas {IP, puerto} cada una correspondiente a un servidor memcached.
%% @return existingClient si ya se habia registrado a un cliente previamente, clientCreated si no se habia registrado previamente y se logro lanzar el proceso cliente exitosamente.
-spec start([util:server_info()]) -> existingClient | clientCreated.
start(ServerList) ->

  Length = length(ServerList),

  case utils:is_registered(client) of 
    yes -> existingClient;
    no  -> case utils:create_sockets(ServerList) of
    
            {createSocketsError, ServerInfo} -> {invalidServer, ServerInfo};

            Sockets -> Servers = [element(1, lists:nth((I rem Length) + 1, Sockets)) || I <- lists:seq(0, Length * ?BUCKET_FACTOR - 1) ],
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


%% @doc Finaliza la ejecucion del cliente y des-registra al alias cliente.
%% @return notExistingClient si no se habia lanzado un cliente aun, o clientClosed si se logra efectivamente finalizar la conexion.
-spec quit() -> notExistingClient | clientClosed.
quit() ->

  case utils:is_registered(client) of 
    
    no -> notExistingClient;  
    
    yes -> client ! quitClient,
           unregister(client),
           clientClosed
  end.
  


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
    Response -> io:fwrite(Response), io:fwrite("~n")
  end.

%% @doc Obtiene las estadisticas de los servidores a los que el cliente se encuentra conectado.
%% @return La respuesta del cliente, que nos muestra un mensaje
stats() ->
  client ! {stats, self() },

  receive 
    Response -> io:fwrite(Response), io:fwrite("~n")
  end.






create_servers_info(Sockets) ->
  [#serverInfo{socket = Socket, id = Id, keyCounter = 0, address = ServerInfo} || {{Socket, ServerInfo }, Id} <- lists:zip(Sockets, lists:seq(1, length(Sockets)))].




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


delete_info(_, []) -> [];
delete_info(ServerSocket, [ServerInfo | ServersInfo]) ->
  case ServerSocket == ServerInfo#serverInfo.socket of
    true  -> delete_info(ServerSocket, ServersInfo);
    false -> [ServerInfo | delete_info(ServerSocket, ServersInfo)]
  end.


%% @doc Dado un ServersTable con la informacion de los servidores corriendo y la informacion de un servidor objetivo, reemplaza cada aparicion del socket del server objetivo en el ServersTable por un socket elegido aleatoriamente de entre los demas sockets de los servers disponibles, rebalanceando asi la carga entre todos los servidores.
%% @param ServersTable La estructura con la informacion de los servidores memcached en ejecucion.
%% @param ServerInfo La informacion del server que queremos reemplazar.
%% @return Un nuevo ServersTable con el rebalanceo de servidores aplicado.
-spec rebalance_servers(servers_table(), gen_tcp:socket()) -> servers_table().
rebalance_servers(ServersTable, ServerSocket) ->
  
  Id               = ServersTable#serversTable.identifier, 
  Size             = ServersTable#serversTable.size, 
  InitialNumServer = ServersTable#serversTable.initial_num_server,
  Servers          = ServersTable#serversTable.servers, 
  ServersInfo      = ServersTable#serversTable.servers_info, 

  NewServersInfo = delete_info(ServerSocket, ServersInfo), % Elimino la informacion del servidor 
  io:fwrite("~p~n", [NewServersInfo]),
  RebalancedServers   = replace_sockets(Servers, ServerSocket, NewServersInfo), % Rebalanceo la carga

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


alter_server_info_counter(ServerInfo, Socket, K) ->
  case ServerInfo#serverInfo.socket == Socket of
      true -> #serverInfo{socket = Socket,
                          id = ServerInfo#serverInfo.id,
                          keyCounter = ServerInfo#serverInfo.keyCounter + K,
                          address = ServerInfo#serverInfo.address };
      false -> ServerInfo
  end.

modify_key_counter(ServersTable, Socket, K) ->
  ServersInfo = ServersTable#serversTable.servers_info,
  NewServersInfo = lists:map(fun(ServerInfo) -> alter_server_info_counter(ServerInfo, Socket, K) end, ServersInfo),

  NewServersTable = #serversTable{ size = ServersTable#serversTable.size,
                                   initial_num_server = ServersTable#serversTable.initial_num_server,
                                   servers = ServersTable#serversTable.servers,
                                   identifier = ServersTable#serversTable.identifier,
                                   servers_info = NewServersInfo },
  NewServersTable.



%% @doc Dada la estructura con la informacion de los servidores memcached corriendo, recibe los pedidos del cliente y los responde indefinidamente, o hasta que se haga un pedido de cierre.
%% @param ServersTable La estructura con informacion de los servidores memcached corriendo.
-spec client(servers_table()) -> term().
client(ServersTable) ->

  receive

    { put, Key, Value, PID } -> 

      {BinaryKey, KeyLength}     = utils:binary_convert(Key),
      {BinaryValue, ValueLength} = utils:binary_convert(Value),

      ServerSocket = get_server(BinaryKey, ServersTable),

      PutMessage = utils:create_message(?PUT, KeyLength, BinaryKey, ValueLength, BinaryValue),
      gen_tcp:send(ServerSocket, PutMessage),

      ResponseCode = utils:recv_bytes(ServerSocket, 1),

      { Response, NewServersTable, Target} = 
        case ResponseCode of
          serverError   -> { { put, Key, Value, PID }, rebalance_servers(ServersTable, ServerSocket), client };
          <<?OK>>       -> { ok, modify_key_counter(ServersTable, ServerSocket, 1), PID }; 
          <<?EBIG>>     -> { ebig, ServersTable, PID }
        end,

      Target ! Response, 

      client(NewServersTable);

    { del, Key, PID } ->

      { BinaryKey, KeyLength} = utils:binary_convert(Key),
      ServerSocket = get_server(BinaryKey, ServersTable),

      DelMessage = utils:create_message(?DEL, KeyLength, BinaryKey),
 
      gen_tcp:send(ServerSocket, DelMessage),

      ResponseCode = utils:recv_bytes(ServerSocket, 1),

     { Response, NewServersTable, Target} = 
        case ResponseCode of
          serverError     -> { ok, rebalance_servers(ServersTable, ServerSocket), PID};
          <<?OK>>         -> { ok,  modify_key_counter(ServersTable, ServerSocket, -1), PID }; 
          <<?ENOTFOUND>> -> { enotfound, ServersTable, PID }
        end,

      Target ! Response, 

      client(NewServersTable);

    { get, Key, PID } ->
      
      {BinaryKey, KeyLength} = utils:binary_convert(Key),
      ServerSocket = get_server(BinaryKey, ServersTable),

      GetMessage = utils:create_message(?GET, KeyLength, BinaryKey),

      gen_tcp:send(ServerSocket, GetMessage),

      ResponseCode = utils:recv_bytes(ServerSocket, 1),

      { Response, NewServersTable, Target } = 
        case ResponseCode of
          serverError    -> { enotfound, rebalance_servers(ServersTable, ServerSocket), PID };
          <<?ENOTFOUND>> -> { enotfound, ServersTable, PID };
          <<?OK>>        -> 
            BinaryLength = utils:recv_bytes(ServerSocket, 4),
            case BinaryLength of
              serverError            -> { enotfound, rebalance_servers(ServersTable, ServerSocket), PID };
              <<ValueLength:32/big>> ->
                  BinaryValue = utils:recv_bytes(ServerSocket, ValueLength),
                  case BinaryValue of
                    serverError -> { enotfound, rebalance_servers(ServersTable, ServerSocket), PID };
                    _           -> { {ok, binary_to_term(BinaryValue) }, ServersTable, PID }    %!! FALTARIA SACARLE EL HEADER DEL ID
                  end
            end
        end,

      Target ! Response, 
      client(NewServersTable);


    { stats, PID } -> 
      StatsMessage = utils:create_stats_message(ServersTable#serversTable.servers_info, ServersTable#serversTable.initial_num_server),
      PID ! StatsMessage,
      client(ServersTable); 

    { status, PID} -> 
      KeyCounters = lists:map(fun(ServerInfo) -> ServerInfo#serverInfo.keyCounter end, ServersTable#serversTable.servers_info),
      TotalKeys = lists:sum(KeyCounters),

      StatusMessage = utils:create_status_message(TotalKeys, ServersTable#serversTable.servers_info, ServersTable#serversTable.initial_num_server), 

      PID ! StatusMessage,
      client(ServersTable);

    quitClient -> close_server_sockets(ServersTable#serversTable.servers_info);


    Other -> 
      error(error),
      io:fwrite("[Error] Invalid request: ~p~n",[Other])

  end.
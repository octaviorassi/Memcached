-module(client).
-export([start/1, put/3, del/2, get/2, stats/1, status/1, startDefault/0, quit/1, client/1]).

-include("protocol.hrl").
-include("common.hrl").

-define(BUCKET_FACTOR, 10).

-define(PRINT(Value), io:format("[Debug] ~p~n", [Value])).


%% @doc Toma una lista de pares IP y puerto correspondientes a servidores memcached, crea el socket para la conexion con cada uno de ellos, y genera el proceso cliente que se utilizara para hacer los pedidos.
%% @param ServerList una lista de tuplas {IP, puerto} cada una correspondiente a un servidor memcached.
%% @return El PID del nuevo cliente creado y que sera usado por las demas funciones.
-spec start([util:server_info()]) -> { invalidServer, server_info() } | pid().
start(ServerList) ->

  Length = length(ServerList),

  case utils:create_sockets(ServerList) of
    
    {createSocketsError, ServerInfo} -> {invalidServer, ServerInfo};

    Sockets -> Servers = [element(1, lists:nth((I rem Length) + 1, Sockets)) || I <- lists:seq(0, Length * ?BUCKET_FACTOR - 1) ],
               ServersInfo = utils:create_servers_info(Sockets),
               
               TimeStamp = erlang:system_time(),
               Identifier = crypto:hash(sha, integer_to_binary(TimeStamp)),
               
               ServersTable = #serversTable{size = Length, initial_num_server = Length, servers = Servers, identifier = Identifier, servers_info = ServersInfo},
               ClientPID = spawn(?MODULE, client, [ServersTable]),
               ClientPID
  end.
  


%% @doc Inicializa el proceso cliente con valores establecidos por defecto. Considera funcionando a dos servidores en la direccion de loopback de IPv4 en los puertos 8000 y 9000.
%% @return existingClient si ya se habia registrado a un cliente previamente, clientCreated si no se habia registrado previamente y se logro lanzar el proceso cliente exitosamente.
-spec startDefault() -> existingClient | clientCreated.
startDefault() ->
  start([{"127.0.0.1", 8000}, {"127.0.0.1", 9000}]).


%% @doc Finaliza la ejecucion de cierto cliente.
%% @param ClientPID el PID del cliente que queremos cerrar.
%% @return clientClosed indicando que se cerro el cliente o nonExistingClient si no existe un cliente con el PID pasado.
-spec quit(pid()) -> clientClosed | nonExistingClient.
quit(ClientPID) -> 
  case is_process_alive(ClientPID) of
    true -> ClientPID ! quitClient,
            clientClosed;
  
    false -> nonExistingClient
  end.
  

%% @doc Almacena el par clave-valor indicado en el servidor memcached asociado a la instancia de cliente con el PID pasado. 
%% @param ClientPID el PID del cliente al cual le queremos hacer una operacion de PUT.
%% @param Key La clave a almacenar.
%% @param Value El valor asociado a la clave.
%% @return La respuesta del cliente o nonExistingClient si no existe un cliente asociado al PID pasado.
-spec put(pid(), term(), term()) -> term() | nonExistingClient.
put(ClientPID, Key, Value) ->
  case is_process_alive(ClientPID) of
    true -> ClientPID ! { put, Key, Value, self() }, 
            receive 
              Response -> Response
            end;

    false -> nonExistingClient
  end.

%% @doc Elimina el par clave-valor asociado a la clave objetivo en la instancia de cliente con el PID pasado.
%% @param ClientPID el PID del cliente al cual le queremos hacer una operacion de DEL.
%% @param Key La clave del par a eliminar.
%% @return La respuesta del cliente o nonExistingClient si no existe un cliente asociado al PID pasado.
-spec del(pid(), term()) -> term() | nonExistingClient.
del(ClientPID, Key) -> 
  case is_process_alive(ClientPID) of
    true -> ClientPID ! { del, Key, self() },
            receive 
              Response -> Response
            end;

    false -> nonExistingClient
  end.

%% @doc Obtiene el valor asociado a la clave objetivo en la instancia de cliente con el PID pasado.
%% @param ClientPID el PID del cliente al cual le queremos hacer una operacion de GET.
%% @param Key La clave del par a buscar.
%% @return La respuesta del cliente o nonExistingClient si no existe un cliente asociado al PID pasado.
-spec get(pid(), term()) -> term() | nonExistingClient.
get(ClientPID, Key) ->
  case is_process_alive(ClientPID) of
    true -> ClientPID ! { get, Key, self() },
            receive 
              Response -> Response
            end;

    false -> nonExistingClient
  end.

%% @doc Obtiene e imprime de las estadisticas que lleva localmente el cliente con el PID pasado.
%% @param ClientPID el PID del cliente al cual le queremos hacer una operacion de STATUS.
%% @return ok o nonExisting client si no existe un cliente asociado al PID pasado.
-spec status(pid()) -> ok | nonExistingClient.
status(ClientPID) -> 
  case is_process_alive(ClientPID) of
    true -> ClientPID ! { status, self() },
            receive 
              Response -> io:fwrite(Response), io:fwrite("~n")
            end;

    false -> nonExistingClient
  end.


%% @doc Obtiene e imprime las estadisticas de los servidores a los que el cliente con el PID pasado se encuentra conectado.
%% @param ClientPID el PID del cliente al cual le queremos hacer una operacion de STATS.
%% @return ok o nonExisting client si no existe un cliente asociado al PID pasado.
-spec stats(pid()) -> ok | nonExistingClient.
stats(ClientPID) ->
  case is_process_alive(ClientPID) of
    true -> ClientPID ! {stats, self() },
            receive 
              Response -> io:fwrite(Response), io:fwrite("~n")
            end;

    false -> nonExistingClient
  end.


%% @doc Dada la estructura con la informacion de los servidores memcached corriendo, recibe los pedidos del cliente y los responde indefinidamente, o hasta que se haga un pedido de cierre.
%% @param ServersTable La estructura con la informacion necesario para que el cliente pueda ejecutarse.
-spec client(servers_table()) -> term().
client(ServersTable) ->

  receive

    { put, Key, Value, PID } -> 

      {BinaryKey, KeyLength}     = utils:binary_convert_key(Key, ServersTable#serversTable.identifier),
      {BinaryValue, ValueLength} = utils:binary_convert_value(Value),

      ServerSocket = utils:get_server(BinaryKey, ServersTable),

      PutMessage = utils:create_message(?PUT, KeyLength, BinaryKey, ValueLength, BinaryValue),
      gen_tcp:send(ServerSocket, PutMessage),

      ResponseCode = utils:recv_bytes(ServerSocket, ?RESPONSE_SIZE),

      { Response, NewServersTable, Target} = 
        case ResponseCode of
          serverError   -> { { put, Key, Value, PID }, utils:rebalance_servers(ServersTable, ServerSocket), self() };
          <<?OK>>       -> { ok, utils:modify_key_counter(ServersTable, ServerSocket, ?INCREASE), PID }; 
          <<?EBIG>>     -> { ebig, ServersTable, PID }
        end,

      Target ! Response, 

      client(NewServersTable);

    { del, Key, PID } ->

      {BinaryKey, KeyLength} = utils:binary_convert_key(Key, ServersTable#serversTable.identifier),
      ServerSocket = utils:get_server(BinaryKey, ServersTable),

      DelMessage = utils:create_message(?DEL, KeyLength, BinaryKey),
 
      gen_tcp:send(ServerSocket, DelMessage),

      ResponseCode = utils:recv_bytes(ServerSocket, ?RESPONSE_SIZE),

     { Response, NewServersTable, Target} = 
        case ResponseCode of
          serverError     -> { ok, utils:rebalance_servers(ServersTable, ServerSocket), PID};
          <<?OK>>         -> { ok,  utils:modify_key_counter(ServersTable, ServerSocket, ?DECREASE), PID }; 
          <<?ENOTFOUND>> -> { enotfound, ServersTable, PID }
        end,

      Target ! Response, 

      client(NewServersTable);

    { get, Key, PID } ->
      
      {BinaryKey, KeyLength} = utils:binary_convert_key(Key, ServersTable#serversTable.identifier),
      ServerSocket = utils:get_server(BinaryKey, ServersTable),

      GetMessage = utils:create_message(?GET, KeyLength, BinaryKey),

      gen_tcp:send(ServerSocket, GetMessage),

      ResponseCode = utils:recv_bytes(ServerSocket, ?RESPONSE_SIZE),

      { Response, NewServersTable, Target } = 
        case ResponseCode of
          serverError    -> { enotfound, utils:rebalance_servers(ServersTable, ServerSocket), PID };
          <<?ENOTFOUND>> -> { enotfound, ServersTable, PID };
          <<?OK>>        -> 
            BinaryLength = utils:recv_bytes(ServerSocket, ?LEN_PREFIX_SIZE),
            case BinaryLength of
              serverError            -> { enotfound, utils:rebalance_servers(ServersTable, ServerSocket), PID };
              <<ValueLength:(?LEN_PREFIX_SIZE_BITS)/big>> ->
                  BinaryValue = utils:recv_bytes(ServerSocket, ValueLength),
                  case BinaryValue of
                    serverError -> { enotfound, utils:rebalance_servers(ServersTable, ServerSocket), PID };
                    _           -> { {ok, binary_to_term(BinaryValue) }, ServersTable, PID }
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

    quitClient -> utils:close_server_sockets(ServersTable#serversTable.servers_info);


    Other -> 
      error(error),
      io:fwrite("[Error] Invalid request: ~p~n",[Other])

  end.

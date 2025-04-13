-module(client).
-export([start/1, put/2, del/1, get/1, stats/0, status/0, startDefault/0, quit/0, client/1]).

-include("protocol.hrl").
-include("common.hrl").

-define(BUCKET_FACTOR, 10).

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
                       ServersInfo = utils:create_servers_info(Sockets),
                       
                       TimeStamp = erlang:system_time(),
                       Identifier = crypto:hash(sha, integer_to_binary(TimeStamp)),
                       
                       ServersTable = #serversTable{size = Length, initial_num_server = Length, servers = Servers, identifier = Identifier, servers_info = ServersInfo},
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

%% @doc Obtiene e imprime de las estadisticas que el cliente lleva localmente.
%% @return ok
-spec status() -> ok.
status() -> 
  client ! { status, self() },
  
  receive 
    Response -> io:fwrite(Response), io:fwrite("~n")
  end.

%% @doc Obtiene e imprime las estadisticas de los servidores a los que el cliente se encuentra conectado.
%% @return ok.
-spec stats() -> ok.
stats() ->
  client ! {stats, self() },

  receive 
    Response -> io:fwrite(Response), io:fwrite("~n")
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
          serverError   -> { { put, Key, Value, PID }, utils:rebalance_servers(ServersTable, ServerSocket), client };
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
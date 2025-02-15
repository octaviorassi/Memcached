-module(client).
-export([start/1, put/2, del/1, get/1, stats/0, startDefault/0, quit/0, client/1]).
-include("protocol.hrl").

-define(BUCKET_FACTOR, 10).
-record(serverMap, {size, servers, identifier, available_sockets}).

-define(PRINT(Value), io:format("[Debug] ~p~n", [Value])).




is_registered(PidAlias) ->
  case whereis(PidAlias) of
    undefined -> no;
    _         -> yes
  end.


quit() ->

  case is_registered(client) of 
    
    no -> notExistingClient;  
    
    yes -> client ! quitClient,
           unregister(client),
           clientClosed
  end.
  


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
  
  

startDefault() ->
  start([{"127.0.0.1", 8000}, {"127.0.0.1", 9000}]).


put(Key, Value) ->

  client ! { put, Key, Value, self() }, 

  receive 
    Response -> Response
  end.


del(Key) -> 

  client ! { del, Key, self() },
  
  receive 
    Response -> Response
  end.

get(Key) ->
  client ! { get, Key, self() },
  
  receive 
    Response -> Response
  end.

stats() ->
  client ! { stats, self() },
  
  receive 
    Response -> Response
  end.


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

get_server(Key, ServerMap) ->

    BinaryHash = crypto:hash(sha, Key),

    MapIndex = binary:decode_unsigned(BinaryHash),
    Index = (MapIndex rem (ServerMap#serverMap.size)) + 1,

    lists:nth(Index, ServerMap#serverMap.servers).


close_server_sockets(ServerSockets) ->
  lists:foreach(fun gen_tcp:close/1, ServerSockets).


client(ServerMap) ->

  receive
    { put, Key, Value, PID } -> 

      {BinaryKey, KeyLength}     = utils:binary_convert(Key),
      {BinaryValue, ValueLength} = utils:binary_convert(Value),

      ServerSocket = get_server(BinaryKey, ServerMap),

      PutMessage = utils:create_message(?PUT, KeyLength, BinaryKey, ValueLength, BinaryValue),

      gen_tcp:send(ServerSocket, PutMessage), % Cambiar por sendn

      Response = utils:recv_bytes(ServerSocket, 1),

      case Response of 
        
        serverError -> client ! { put, Key, Value, PID},                    % Reenviamos el mensaje
                       client(rebalance_servers(ServerMap, ServerSocket));  % Rebalancear la carga
        <<?OK>>   -> PID ! ok;
        <<?EBIG>> -> PID ! ebig 
      end,

      client(ServerMap);

    { del, Key, PID } ->

      { BinaryKey, KeyLength} = utils:binary_convert(Key),
      ServerSocket = get_server(BinaryKey, ServerMap),

      DelMessage = utils:create_message(?DEL, KeyLength, BinaryKey),

      gen_tcp:send(ServerSocket, DelMessage),

      Response = utils:recv_bytes(ServerSocket, 1),

      case  Response of 
        serverError -> 
          PID ! enotfound,
          client(rebalance_servers(ServerMap, ServerSocket)); % Estaria bueno que avise
        <<?OK>>   -> PID ! ok;
        <<?ENOTFOUND>> -> PID ! enotfound 
      end,

      % Volvemos a entrar en loop
      client(ServerMap);

    { get, Key, PID } ->
      
      {BinaryKey, KeyLength} = utils:binary_convert(Key),
      ServerSocket = get_server(BinaryKey, ServerMap),

      GetMessage = utils:create_message(?GET, KeyLength, BinaryKey),

      gen_tcp:send(ServerSocket, GetMessage),
      
      Response = utils:recv_bytes(ServerSocket, 1),

      case Response of
        serverError -> 
          PID ! enotfound,
          client(rebalance_servers(ServerMap, ServerSocket)); % Estaria bueno que avise
        <<?OK>>   -> 
          <<ValueLength:32/big>> = utils:recv_bytes(ServerSocket, 4),
          BinaryValue = utils:recv_bytes(ServerSocket, ValueLength),
          Value = binary_to_term(BinaryValue),
          PID ! {ok, Value};

        <<?ENOTFOUND>> -> PID ! enotfound 
      end,

      % Volvemos a entrar en loop
      client(ServerMap);

    { stats, PID } -> PID;


    quitClient -> close_server_sockets(ServerMap#serverMap.available_sockets);

    Other -> 
      error(error),
      io:fwrite("[Error] Invalid request: ~p~n",[Other])
  end.



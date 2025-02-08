-module(client).
-export([start/1, put/2, del/1, get/1, stats/0, client/1, startDefault/0]).
-include("protocol.hrl").

-define(BUCKET_FACTOR, 10).
-record(serverMap, {size, servers, identifier}).

-define(PRINT(Value), io:format("[Debug] ~p~n", [Value])).


start(ServerList) ->

  Length = length(ServerList),
  Sockets = utils:create_sockets(ServerList), % Ver como administrar errores
  
  Servers = [lists:nth((I rem Length) + 1, Sockets) || I <- lists:seq(0, Length * ?BUCKET_FACTOR - 1)],
  ServerMap = #serverMap{size = Length, servers = Servers},
  
  ClientPID = spawn(?MODULE, client, [ServerMap]),
  
  register(client, ClientPID), % Lo registramos globalmente
  
  clientCreated. % Salio todo OK
  

startDefault() ->
  start([{"127.0.0.1", 889}]).


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





client(ServerMap) ->

  % Cada parte del receive la podemos modularizar y quedaria mejor y mas simple

  receive
    { put, Key, Value, PID } -> 

      {BinaryKey, KeyLength}     = utils:binary_convert(Key),
      {BinaryValue, ValueLength} = utils:binary_convert(Value),

      ServerSocket = get_server(BinaryKey, ServerMap),

      PutMessage = utils:create_message(?PUT, KeyLength, BinaryKey, ValueLength, BinaryValue),

      gen_tcp:send(ServerSocket, PutMessage),
      Response = utils:recv_bytes(ServerSocket, 1),

      case  Response of 
        <<?OK>>   -> PID ! ok;
        <<?EBIG>> -> PID ! ebig 
      end,

      client(ServerMap);

    { del, Key, PID } ->

      {BinaryKey, KeyLength} = utils:binary_convert(Key),
      ServerSocket = get_server(BinaryKey, ServerMap),

      DelMessage = utils:create_message(?DEL, KeyLength, BinaryKey),

      gen_tcp:send(ServerSocket, DelMessage),

      Response = utils:recv_bytes(ServerSocket, 1),

      case  Response of 
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

      case  Response of 
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

    Other -> 
      error(error),
      io:fwrite("[Error] Invalid request: ~p~n",[Other])
  
  end.








get_server(Key, ServerMap) ->

    BinaryHash = crypto:hash(sha, Key),

    MapIndex = binary:decode_unsigned(BinaryHash),
    Index = (MapIndex rem (ServerMap#serverMap.size)) + 1,

    lists:nth(Index, ServerMap#serverMap.servers).


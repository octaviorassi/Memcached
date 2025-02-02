-module(client).
-export([start/1, put/2, del/1, get/1, stats/0, client/1]).
-include("protocol.hrl").

-define(PRINT(Value), io:format("[Debug] ~p~n", [Value])).

-define(BUCKET_FACTOR, 10).

-record(serverMap, {size, servers}).



create_socket(ServerInfo) ->
  {IpAddress, Port} = ServerInfo,
  {ok, Socket} = socket:open(inet, stream, tcp),
  ok = socket:connect(Socket, #{family => inet,
                                addr   => string_to_ip(IpAddress),
                                port   => Port}),
  Socket.


create_sockets(ServerList) -> 
  lists:map(fun create_socket/1, ServerList). % A cada par IP,Puerto le asignamos un socket


string_to_ip(StringIP) -> 
  {ok, IpAddress} = inet:parse_address(StringIP),
  IpAddress.

%                     0          1              n
% ServerList :: [ {ip0, p0}, {ip1, p1},..., {ipn, pn}] 
start(ServerList) ->

  % Eliminamos repetidos de ServerList (Opcional, podrias desear que tengas mas 
  % referencias a determinado Server.

  Length = length(ServerList),
  
  % Creamos tantos sockets como servidores nos hayan pasado y los ponemos en una lista
  Sockets = create_sockets(ServerList),
  
  % Hacemos la lista de Buckets, en donde a cada uno le corresponde un socket
  % su posicion, modulo Length * BUCKET_FACTOR en la lista de Sockets
  Servers = [lists:nth((I rem Length) + 1, Sockets) || I <- lists:seq(0, Length * ?BUCKET_FACTOR - 1)],

  ServerMap = #serverMap{size = Length, servers = Servers},
  ClientPID = spawn(?MODULE, client, [ServerMap]),
  
  register(client, ClientPID),
  clientCreated.
  

put(Key, Value) -> 
  BinaryKey = term_to_binary(Key),
  BinaryValue = term_to_binary(Value),
  client ! {put, BinaryKey, BinaryValue, self()},
  receive
    {ok, Result} -> ?PRINT(Result)
  end.

del(Key) -> 
  BinaryKey = term_to_binary(Key),
  clientPID ! {del, BinaryKey, self()}.

get(Key) -> 
  BinaryKey = term_to_binary(Key),
  clientPID ! {del, BinaryKey, self()}.

stats() -> 
  clientPID ! {stats, self()}.

% Funcion que ejecuta el proceso cliente
client(ServerMap) -> 

  receive
    {put, Key, Value, PID}-> 
      KeyLength = byte_size(Key),
      ValueLength = byte_size(Value),

      BinaryHash = crypto:hash(sha, Key),
      MapIndex = binary:decode_unsigned(BinaryHash),
      ?PRINT(MapIndex),
      ?PRINT(ServerMap#serverMap.size),
      ServerSocket = lists:nth((MapIndex rem (ServerMap#serverMap.size)) + 1, ServerMap#serverMap.servers),

      Message = <<?PUT:8/integer, KeyLength:32/integer, Key/binary, ValueLength:32/integer, Value/binary>>,

      ?PRINT(binary_to_list(Message)),
      socket:send(ServerSocket, Message),
      {ok, Respuesta} = socket:recv(ServerSocket),
      PID ! {ok, Respuesta};

    {del, Key, PID}-> ok;
    {get, Key, PID}-> ok;
    {stats, PID}-> ok;
    _ -> error(error) % No matchea ningun mensaje correcto
  end.
  



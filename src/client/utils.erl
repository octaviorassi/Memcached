-module(utils).
-export([create_sockets/1, binary_convert/1, recv_bytes/2, create_message/1, create_message/3, create_message/5]).


recv_bytes(Socket, N) -> recv_bytes_aux(Socket, N, <<>>).

recv_bytes_aux(_, 0, Buffer) -> Buffer; % Ya dejamos de leer 


recv_bytes_aux(Socket, N, Buffer) -> 
  case gen_tcp:recv(Socket, N) of
    {ok, Message} -> 
      recv_bytes_aux(Socket, N - byte_size(Message), <<Message/binary, Buffer/binary>>)
  end. 
  

binary_convert(Term) ->
  BinaryTerm = term_to_binary(Term),
  BinaryLength = byte_size(BinaryTerm),
  {BinaryTerm, BinaryLength}. 


create_socket(ServerInfo) ->
  
  {IpAddress, Port} = ServerInfo,
  Options = [binary, {active, false}, inet, {packet, 0}],
  {ok, Socket} = gen_tcp:connect(IpAddress, Port, Options),
  Socket.


create_sockets(ServerList) -> 
  lists:map(fun create_socket/1, ServerList). % A cada par IP,Puerto le asignamos un socket


create_message(Operation, KeyLength, Key) ->
  <<Operation:8/integer, KeyLength:32/integer, Key/binary>>.

create_message(Operation, KeyLength, Key, ValueLength, Value) ->
  <<Operation:8/integer, KeyLength:32/integer, Key/binary, ValueLength:32/integer, Value/binary>>.

create_message(Operation) ->
  <<Operation:8/integer>>.

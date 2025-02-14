-module(utils).
-export([create_sockets/1, binary_convert/1, recv_bytes/2, create_message/1, create_message/3, create_message/5]).


recv_bytes(Socket, N) -> recv_bytes_aux(Socket, N, <<>>).

recv_bytes_aux(_     , 0, Buffer) -> Buffer; 
recv_bytes_aux(Socket, N, Buffer) -> 
 
  case gen_tcp:recv(Socket, N) of
    {ok, Message} -> 
      recv_bytes_aux(Socket, N - byte_size(Message), <<Message/binary, Buffer/binary>>);

    {error, _} -> serverError % No nos importa mucho la razon, es un error
  end. 

  
binary_convert(Term) ->
  BinaryTerm = term_to_binary(Term),
  BinaryLength = byte_size(BinaryTerm),
  {BinaryTerm, BinaryLength}. 


create_socket(ServerInfo) ->

  {IpAddress, Port} = ServerInfo,
  Options = [binary, {active, false}, inet, {packet, 0}],
  
  case gen_tcp:connect(IpAddress, Port, Options) of 

    {ok, Socket} -> Socket;
    {error, _}   -> { error, ServerInfo }

  end.


all_created([])                         -> allCreated;
all_created([{ error, ServerInfo} | _]) -> { notCreated, ServerInfo };
all_created([_ | Sockets])              -> all_created(Sockets).

create_sockets(ServerList) -> 

  SocketList = lists:map(fun create_socket/1, ServerList),
  case all_created(SocketList) of
    allCreated               -> SocketList;
    {notCreated, ServerInfo} -> {createSocketsError, ServerInfo}
  end.


create_message(Operation, KeyLength, Key) ->
  <<Operation:8/integer, KeyLength:32/integer, Key/binary>>.

create_message(Operation, KeyLength, Key, ValueLength, Value) ->
  <<Operation:8/integer, KeyLength:32/integer, Key/binary, ValueLength:32/integer, Value/binary>>.

create_message(Operation) ->
  <<Operation:8/integer>>.

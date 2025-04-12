-module(utils).
-export([is_registered/1, create_sockets/1, binary_convert/1, recv_bytes/2, create_message/1,  create_message/3, create_message/5, create_stats_message/2, create_status_message/3]).
-include("protocol.hrl").


-type server_info() :: {string(), non_neg_integer()}.

%% @doc Lee N bytes del socket objetivo.
%% @param Socket objeto Socket de gen_tcp del cual queremos leer.
%% @param N entero no negativo que representa la cantidad de bytes a leer.
%% @return {ok, binary()} si se leyo correctamente, {error, _} si no.
-spec recv_bytes(gen_tcp:socket(), non_neg_integer()) -> {ok, binary()} | {error, term()}.
recv_bytes(Socket, N) -> recv_bytes_aux(Socket, N, <<>>).


%% @doc Funcion auxiliar para recv_bytes que agrega un buffer donde leer el mensaje.
%% @param Socket objeto Socket de gen_tcp del cual queremos leer.
%% @param N entero no negativo que representa la cantidad de bytes a leer.
%% @param Buffer buffer de tipo binary donde se lee el mensaje.
%% @return {ok, binary()} si se leyo correctamente, {error, _} si no.
-spec recv_bytes_aux(gen_tcp:socket(), non_neg_integer(), binary()) -> {ok, binary()} | {error, term()}.
%% 
recv_bytes_aux(_     , 0, Buffer) -> Buffer; 
recv_bytes_aux(Socket, N, Buffer) -> 
 
  case gen_tcp:recv(Socket, N) of
    {ok, Message} -> 
      recv_bytes_aux(Socket, N - byte_size(Message), <<Message/binary, Buffer/binary>>);

    {error, _} -> serverError % No nos importa mucho la razon, es un error
  end. 

  
%% @doc Toma un termino, lo convierte a binario, y lo retorna junto con su longitud.
%% @param Term el termino a convertir.
%% @return Una tupla {BinaryTerm, BinaryLength} con el binario equivalente al termino y su longitud.
-spec binary_convert(term()) -> {binary(), non_neg_integer()}.
binary_convert(Term) ->
  BinaryTerm = term_to_binary(Term),
  BinaryLength = byte_size(BinaryTerm),
  {BinaryTerm, BinaryLength}. 


%% @doc Toma un par {IP, Puerto} e intenta establecer una conexion TCP en protocolo binario.
%% @param ServerInfo la tupla {IP, Puerto} asociada al servidor al que queremos conectarnos.
%% @return Retorna el objeto gen_tcp:socket() en caso de conectarse exitosamente, o una tupla {error, ServerInfo} con la informacion del servidor al que no logro conectarse.
-spec create_socket(server_info()) -> { gen_tcp:socket(), server_info() } | {error, server_info()}. 
create_socket(ServerInfo) ->

  {IpAddress, Port} = ServerInfo,
  Options = [binary, {active, false}, inet, {packet, 0}],
  
  case gen_tcp:connect(IpAddress, Port, Options) of 

    {ok, Socket} -> { Socket, ServerInfo };
    {error, _}   -> { error, ServerInfo }

  end.


%% @doc Dada una lista generada por create_sockets, chequea que no se haya producido ningun error en la creacion de los sockets.
%% @param List la lista de Sockets o posibles tuplas de error.
%% @return allCreated si toda la lista contiene objetos gen_tcp:socket(), o una tupla {notCreated, ServerInfo} indicando que el servidor con informacion ServerInfo fracaso en el intento de conexion.
-spec all_created([gen_tcp:socket() | {error, server_info()}]) -> allCreated | {notCreated, server_info()}.
all_created([])                         -> allCreated;
all_created([{ error, ServerInfo} | _]) -> { notCreated, ServerInfo };
all_created([_ | Sockets])              -> all_created(Sockets).


%% @doc Toma una lista de pares IP y puerto, y crea un socket para cada uno de ellos. Si todos se crean correctamente, se devuelve la lista con los objetos gen_tcp:Socket asociados a cada uno.
%% @param ServerList Una lista de pares (string(), non_neg_integer()) representando pares (IP, puerto) de servidores memcached.
%% @return Una lista de gen_tcp:socket() si la creacion es exitosa, o {notCreated, (string(), non_neg_integer())} si se produjo un error, donde la segunda componente de la tupla es el par (IP, puerto) del servidor que no logro conectarse.
-spec create_sockets([server_info()]) -> allCreated | {notCreated, {string(), non_neg_integer()}}.
%% 
create_sockets(ServerList) -> 

  SocketList = lists:map(fun create_socket/1, ServerList),
  case all_created(SocketList) of
    allCreated               -> SocketList;
    {notCreated, ServerInfo} -> {createSocketsError, ServerInfo}
  end.


%% @doc Dado un codigo de operacion, la longitud de la clave, y la clave, genera el binario en el formato especificado por el protocolo que se enviara como mensaje al servidor.
%% @param Operation codigo de la operacion, un entero que debe caber en 8 bits.
%% @param KeyLength la longitud de la clave del mensaje, un entero que debe caber en 32 bits.
%% @param Key la clave en formato binario, con longitud KeyLength.
%% @return El binario en el formato correcto.
-spec create_message(integer(), integer(), binary()) -> binary().
create_message(Operation, KeyLength, Key) ->
  <<Operation:8/integer, KeyLength:32/integer, Key/binary>>.


%% @doc Dado un codigo de operacion, la longitud de la clave, la clave, la longitud del valor, y el valor, genera el binario en el formato especificado por el protocolo que se enviara como mensaje al servidor.
%% @param Operation codigo de la operacion, un entero que debe caber en 8 bits.
%% @param KeyLength la longitud de la clave del mensaje, un entero que debe caber en 32 bits.
%% @param Key la clave en formato binario, con longitud KeyLength.
%% @param ValueLength la longitud del valor del mensaje, un entero que debe caber en 32 bits.
%% @param Value el valor en formato binario, con longitud ValueLength.
%% @return El binario en el formato correcto.
-spec create_message(integer(), integer(), binary(), integer(), binary()) -> binary().
create_message(Operation, KeyLength, Key, ValueLength, Value) ->
  <<Operation:8/integer, KeyLength:32/integer, Key/binary, ValueLength:32/integer, Value/binary>>.


%% @doc Dado un codigo de operacion genera el binario en el formato especificado por el protocolo que se enviara como mensaje al servidor.
%% @param Operation codigo de la operacion, un entero que debe caber en 8 bits.
%% @return El binario en el formato correcto.
-spec create_message(integer()) -> binary().
create_message(Operation) ->
  <<Operation:8/integer>>.



stats_requests(Id, ServersInfo) -> 
  case lists:keyfind(Id, 3, ServersInfo) of 
    
    {_, Socket, _, _, {Ip, Port}} -> 
      StatsMessage = utils:create_message(?STATS),
      gen_tcp:send(Socket, StatsMessage),
      Response = utils:recv_bytes(Socket, 1),

      case Response of
        serverError -> io_lib:format("Server ~p: shutdown", [Id]);
        <<?OK>>     -> 
          <<ValueLength:32/big>> = utils:recv_bytes(Socket, 4), 
          BinaryValue = utils:recv_bytes(Socket, ValueLength),
          String = binary_to_list(BinaryValue),
          io_lib:format("Server ~p (~s:~p): ~p", [Id, Ip, Port, String])
      end;
      
    false          -> io_lib:format("Server ~p: shutdown", [Id])
  end.

create_stats_message(ServersInfo, InitialNumServer) ->
  IndexList = lists:seq(1, InitialNumServer),
  ServersStatsMessage = lists:map(fun(Id) -> stats_requests(Id, ServersInfo) end, IndexList),
  StatsMessage = string:join(ServersStatsMessage, "~n"),
  StatsMessage.

create_status_message(TotalKeys, ServersInfo, InitialNumServer) ->
  
  case TotalKeys of
    0 -> "The client has 0 keys across all the servers~n";
    _ -> 
      IndexList = lists:seq(1, InitialNumServer),
      ServerCounterMessage = 
        lists:map(fun(Id) ->
          case lists:keyfind(Id, 3, ServersInfo) of
            {_,_, Id, PutCounter, {Ip, Port}} -> io_lib:format("Server ~p (~s:~p): ~.2f% Keys", [Id, Ip, Port, PutCounter/TotalKeys * 100]);
            false              -> io_lib:format("Server ~p: shutdown", [Id])
          end
        end, IndexList),

      StatusMessage = string:join(ServerCounterMessage, "~n"),
      StatusMessage
  end.
  
  

%% @doc Determina si el alias para un PID ya ha sido registrado previamente.
%% @param PidAlias el alias cuyo registro queremos chequear.
%% @return no si no estaba registrado, si en caso contrario.
-spec is_registered(atom()) -> no | yes.
is_registered(PidAlias) ->
  case whereis(PidAlias) of
    undefined -> no;
    _         -> yes
  end.
-module(utils).
-export([create_sockets/1, binary_convert_value/1, binary_convert_key/2,
         recv_bytes/2, create_message/1,  create_message/3, create_message/5, create_stats_message/2,
         create_status_message/3, create_servers_info/1, replace_sockets/3, delete_info/2, 
         rebalance_servers/2, get_server/2, close_server_sockets/1, modify_key_counter/3]).

-include("protocol.hrl").
-include("common.hrl").

-define(PREFIX_LENGTH, 20). 


-type server_address() :: {string(), non_neg_integer()}.

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
recv_bytes_aux(_     , 0, Buffer) -> Buffer; 
recv_bytes_aux(Socket, N, Buffer) -> 
 
  case gen_tcp:recv(Socket, N) of
    
    {ok, Message} ->
      % Seguimos recibiendo hasta llegar a 0 
      recv_bytes_aux(Socket, N - byte_size(Message), <<Buffer/binary, Message/binary>>);

    {error, _} -> serverError
  end. 


%% @doc Toma un termino, lo convierte a binario, y lo retorna junto con su longitud.
%% @param Term el termino a convertir.
%% @return Una tupla {BinaryTerm, BinaryLength} con el binario equivalente al termino y su longitud.
-spec binary_convert_value(term()) -> {binary(), non_neg_integer()}.
binary_convert_value(Term) ->
  BinaryTerm = term_to_binary(Term),
  BinaryLength = byte_size(BinaryTerm),
  {BinaryTerm, BinaryLength}. 


%% @doc Toma un termino, lo convierte a binario agregandole el identificador como prefijo, y lo retorna junto con su longitud (considerando el prefijo).
%% @param Term el termino a convertir.
%% @param Identificar el identificador que usaremos como prefijo.
%% @return Una tupla {BinaryTerm, BinaryLength} con el binario equivalente al termino y su longitud.
-spec binary_convert_key(term(), binary()) -> {binary(), non_neg_integer()}.
binary_convert_key(Term, Identifier) ->
  BinaryTerm = term_to_binary(Term),
  BinaryLength = byte_size(BinaryTerm) + ?PREFIX_LENGTH,
  { <<Identifier/binary, BinaryTerm/binary>>, BinaryLength}. 


%% @doc Toma un par {IP, Puerto} e intenta establecer una conexion TCP en protocolo binario.
%% @param ServerInfo la tupla {IP, Puerto} asociada al servidor al que queremos conectarnos.
%% @return Retorna el objeto gen_tcp:socket() en caso de conectarse exitosamente, o una tupla {error, ServerInfo} si no se pudo conectar.
-spec create_socket(server_address()) -> { gen_tcp:socket(), server_address() } | {error, server_address()}. 
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
-spec all_created([gen_tcp:socket() | {error, server_address()}]) -> allCreated | {notCreated, server_address()}.
all_created([])                         -> allCreated;
all_created([{ error, ServerInfo} | _]) -> { notCreated, ServerInfo };
all_created([_ | Sockets])              -> all_created(Sockets).


%% @doc Toma una lista de pares IP y puerto, y crea un socket para cada uno de ellos. Si todos se crean correctamente, se devuelve la lista con los objetos gen_tcp:Socket asociados a cada uno.
%% @param ServerList Una lista de pares (string(), non_neg_integer()) representando pares (IP, puerto) de servidores memcached.
%% @return Una lista de gen_tcp:socket() si la creacion es exitosa, o {notCreated, (string(), non_neg_integer())} si se produjo un error, donde la segunda componente de la tupla es el par (IP, puerto) del servidor que no logro conectarse.
-spec create_sockets([server_address()]) -> allCreated | {notCreated, {string(), non_neg_integer()}}.
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
  <<Operation:(?COMMAND_SIZE_BITS)/integer, KeyLength:(?LEN_PREFIX_SIZE_BITS)/integer, Key/binary>>.


%% @doc Dado un codigo de operacion, la longitud de la clave, la clave, la longitud del valor, y el valor, genera el binario en el formato especificado por el protocolo que se enviara como mensaje al servidor.
%% @param Operation codigo de la operacion, un entero que debe caber en 8 bits.
%% @param KeyLength la longitud de la clave del mensaje, un entero que debe caber en 32 bits.
%% @param Key la clave en formato binario, con longitud KeyLength.
%% @param ValueLength la longitud del valor del mensaje, un entero que debe caber en 32 bits.
%% @param Value el valor en formato binario, con longitud ValueLength.
%% @return El binario en el formato correcto.
-spec create_message(integer(), integer(), binary(), integer(), binary()) -> binary().
create_message(Operation, KeyLength, Key, ValueLength, Value) ->
  <<Operation:(?COMMAND_SIZE_BITS)/integer, KeyLength:(?LEN_PREFIX_SIZE_BITS)/integer, Key/binary, ValueLength:(?LEN_PREFIX_SIZE_BITS)/integer, Value/binary>>.


%% @doc Dado un codigo de operacion genera el binario en el formato especificado por el protocolo que se enviara como mensaje al servidor.
%% @param Operation codigo de la operacion, un entero que debe caber en 8 bits.
%% @return El binario en el formato correcto.
-spec create_message(integer()) -> binary().
create_message(Operation) ->
  <<Operation:(?COMMAND_SIZE_BITS)/integer>>.


%% @doc Envia un pedido de STATS al servidor con cierto identificador y arma un mensaje adecuado.
%% @param Id identificador del servidor al cual le queremos pedir las stats.
%% @param ServersInfo Lista de servidores a los cuales el cliente se encuentra conectado actualmente.
%% @return String que representa el mensaje de stats del server, o que indica que el servidor esta desconectado. 
-spec stats_requests(non_neg_integer(), [server_info()]) -> string().
stats_requests(Id, ServersInfo) -> 
  case lists:keyfind(Id, 3, ServersInfo) of 
    
    {_, Socket, _, _, {Ip, Port}} -> 
      StatsMessage = utils:create_message(?STATS),
      gen_tcp:send(Socket, StatsMessage),
      Response = utils:recv_bytes(Socket, 1),

      case Response of
        serverError -> io_lib:format("Server ~p: disconnected", [Id]);
        <<?OK>>     -> 
          <<ValueLength:(?LEN_PREFIX_SIZE_BITS)/big>> = utils:recv_bytes(Socket, (?LEN_PREFIX_SIZE)), 
          BinaryValue = utils:recv_bytes(Socket, ValueLength),
          String = binary_to_list(BinaryValue),
          io_lib:format("Server ~p (~s:~p): ~p", [Id, Ip, Port, String])
      end;
      
    false          -> io_lib:format("Server ~p: disconnected", [Id])
  end.


%% @doc Crea un string con informacion las stats de los servidores a los que el cliente se encuentra conectado. Si un servidor se desconecta en el momento que pide stats, no hace rebalanceo de cargas (solo lo hacen PUT, DEL, GET).
%% @param ServersInfo Lista de servidores a los cuales el cliente se encuentra conectado actualmente.
%% @param InitialNumServer cantidad de servidores que habia cuando el cliente se creo.
%% @return String que representa el mensaje de stats solicitado.
-spec create_stats_message([server_info()], non_neg_integer()) -> string().
create_stats_message(ServersInfo, InitialNumServer) ->
  IndexList = lists:seq(1, InitialNumServer),
  ServersStatsMessage = lists:map(fun(Id) -> stats_requests(Id, ServersInfo) end, IndexList),
  StatsMessage = string:join(ServersStatsMessage, "~n"),
  StatsMessage.


%% @doc Crea un string con informacion de las stats que el cliente lleva localmente. La funcion sigue contando las claves de un server cuando este se desconecto y no se hizo un rebalanceo de carga.
%% @param TotalKeys numero total de claves.
%% @param Lista de servidores a los cuales el cliente se encuentra conectado actualmente.
%% @param InitialNumServer cantidad de servidores que habia cuando el cliente se creo.
%% @return String que representa el mensaje de status solicitado.
-spec create_status_message(non_neg_integer(), [server_info()], non_neg_integer()) -> string().
create_status_message(TotalKeys, ServersInfo, InitialNumServer) ->
  
  case TotalKeys of
    0 -> "The client has 0 keys across all the servers~n";
    _ -> 
      IndexList = lists:seq(1, InitialNumServer),
      ServerCounterMessage = 
        lists:map(fun(Id) ->
          case lists:keyfind(Id, 3, ServersInfo) of
            {_,_, Id, PutCounter, {Ip, Port}} -> io_lib:format("Server ~p (~s:~p): ~.2f% Keys", [Id, Ip, Port, PutCounter/TotalKeys * 100]);
            false              -> io_lib:format("Server ~p: disconnected", [Id])
          end
        end, IndexList),

      StatusMessage = string:join(ServerCounterMessage, "~n"),
      StatusMessage
  end.


%% @doc Dado un listado de sockets y su direccion y puerto, nos devuelve una lista con la inforamcion que iremos llevando de los sockets.
%% @param Sockets listado de sockets para los cuales queremos crear su lista de informacion.
%% @return Una lista con la informacion asociada a los sockets.
-spec create_servers_info([{gen_tcp:socket(), server_address()}]) -> [server_info()].
create_servers_info(Sockets) ->
  [#serverInfo{ socket = Socket, id = Id, keyCounter = 0, address = ServerInfo} ||
             {{ Socket, ServerInfo }, Id} <- lists:zip(Sockets, lists:seq(1, length(Sockets)))].


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


%% @doc Dado un socket de un servidor y uan lista de informacion de servidores, elimina de la lista aquella informacion de servidor que coincida con el socket pasado.
%% @param ServerSocket El socket de una informacion de servidor que queremos eliminar.
%% @param ServersInfo La lista de informacion de servidores de la cual queremos eliminar aquella que tenga el socket indicado.
%% @return Una nueva lista de informacion de servidores sin las que tenian el socket pasado.
-spec delete_info(gen_tcp:socket(), [server_info()]) -> [server_info()].
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
  lists:foreach(fun({_,ServerSocket,_,_,_}) -> gen_tcp:close(ServerSocket) end, ServersInfo).


%% @doc Dada una informacion de servidor, modifica el contador que lleva por la cantidad K unicamente si coincide con el socket pasado.
%% @param ServerInfo informacion de servidores para la cual queremos modificar su contador, si es que coincide con el socket pasado.
%% @param Socket es aquel al cual le queremos modificar el contador.
%% @param K es el valor por el cual queremos modificar el contador.
%% @return Una nueva informacion de servidor (con el contador modificado por K) si el socket era el correto, si no la misma informacion de servidor.
-spec alter_server_info_counter(server_info(), gen_tcp:socket(), non_neg_integer()) -> server_info().
alter_server_info_counter(ServerInfo, Socket, K) ->
  case ServerInfo#serverInfo.socket == Socket of
      true -> #serverInfo{socket = Socket,
                          id = ServerInfo#serverInfo.id,
                          keyCounter = ServerInfo#serverInfo.keyCounter + K,
                          address = ServerInfo#serverInfo.address };
      false -> ServerInfo
  end.


%% @doc Dada una tabla de servidores, un socket y un valor K, intenta modificar el contador del socket pasado (de la lista de de servidores) por dicha cantidad.
%% @param ServersTable tabla de servidores para la cual queremos modificar el contador de uno de sus sockets.
%% @param Socket es aquel al cual le queremos modificar el contador.
%% @param K es el valor por el cual queremos modificar el contador.
%% @return Una nuevas tablas de servidores, con el contador asociado al socket indicado modificado.
-spec modify_key_counter(servers_table(), gen_tcp:socket(), non_neg_integer()) -> servers_table().
modify_key_counter(ServersTable, Socket, K) ->
  ServersInfo = ServersTable#serversTable.servers_info,
  NewServersInfo = lists:map(fun(ServerInfo) -> alter_server_info_counter(ServerInfo, Socket, K) end, ServersInfo),

  NewServersTable = #serversTable{ size = ServersTable#serversTable.size,
                                   initial_num_server = ServersTable#serversTable.initial_num_server,
                                   servers = ServersTable#serversTable.servers,
                                   identifier = ServersTable#serversTable.identifier,
                                   servers_info = NewServersInfo },
  NewServersTable.
% Records, tipos y constante que utilizaremos en los distintos archivos

% serverInfo: Define la informacion relevante que llevamos sobre un servidor.
-record(serverInfo, {socket, id, keyCounter, address}).
-type server_info() :: #serverInfo { socket     :: gen_tcp:socket(),
                                     id         :: non_neg_integer(),
                                     keyCounter :: non_neg_integer(),
                                     address    :: {string(), non_neg_integer()} }.

% serversTable: Define la informacion que necesita el cliente para administrar los pedidos.
-record(serversTable, {size, initial_num_server, servers, identifier, servers_info}).
-type servers_table() :: #serversTable{ size :: non_neg_integer(),
                                        initial_num_server :: non_neg_integer(),
                                        servers :: [gen_tcp:socket()],
                                        identifier :: binary(),
                                        servers_info :: [server_info()] }.


% Definiciones para incrementar y decremantar los contadores
-define(INCREASE, 1).
-define(DECREASE, -1).
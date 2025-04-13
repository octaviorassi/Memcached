% Comandos del protocolo binario 
-define(PUT, 11).
-define(DEL, 12).
-define(GET, 13).
-define(STATS, 21).

% Respuestas del protocolo binario
-define(OK, 101).
-define(EINVAL, 111).
-define(ENOTFOUND, 112).
-define(EBINARY, 113).
-define(EBIG, 114).
-define(EUNK, 115).

% Cantidad de bytes y bits con los que representamos la longitud de un KEY o VALUE
-define(LEN_PREFIX_SIZE, 4).
-define(LEN_PREFIX_SIZE_BITS, ?LEN_PREFIX_SIZE * 8).

% Cantidad de bytes con los que representamos un comando o una respuesta
-define(COMMAND_SIZE, 1).
-define(COMMAND_SIZE_BITS, ?COMMAND_SIZE * 8).
-define(RESPONSE_SIZE, 1).
-define(RESPONSE_SIZE_BITS, ?RESPONSE_SIZE * 8).
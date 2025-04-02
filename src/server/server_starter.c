#include "server_starter_utils.h"

/**
 *  @brief Lee los argumentos de entrada como numero de puerto, cantidad de memoria, y numero de threads, e inicializa el socket del servidor MemCached, que es pasado por argumento al servidor que ejecuta inmediatamente despues.
 * 
 *  `Uso`:
 * 
 *      [-p port] o [--port port]  para especificar el puerto que escuchara el server.
 * 
 *      [-m mem] o [--memory mem]  para especificar la cantidad de memoria maxima en bytes.
 * 
 *      [-t num] o [--threads num] para especificar la cantidad de threads a lanzar.
 *  
 * 
 *  @return 0 en caso de ejecutar correctamente, distinto de 0 si no.
 * 
 */
int main(int argc, char** argv) {

  Args args;

  if (parse_arguments(argc, argv, &args)) 
    return 1; 
  
  printf("[Port] %d\n", args.port);
  printf("[Memory] %ld Bytes\n", args.memory_limit);
  printf("[Threads] %d\n", args.num_threads);

  Socket server_socket = create_server_socket(args.port);
  
  set_memory_limit(args.memory_limit);

  exec_server("./bin/cache_server", server_socket, args.num_threads);
 
  return 0;
}

#include "server_starter_utils.h"

int main(int argc, char** argv) {

  Args args;

  if (parse_arguments(argc, argv, &args)) 
    return 1; 
  
  printf("[Port] %d\n", args.port);
  printf("[Memory] %ld Bytes\n", args.memory_limit);
  printf("[Threads] %d\n", args.num_threads);

  Socket server_socket = create_server_socket(args.port);
  
  set_memory_limit(args.memory_limit);

  // Bajar privilegios o algo por el estilo

  exec_server("../../bin/cache_server", server_socket, args.num_threads);
 
  return 0;
}

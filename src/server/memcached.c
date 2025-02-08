#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <string.h>
#include <sys/types.h>
#include <netinet/ip.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/epoll.h>
#include <netinet/in.h>
#include <wait.h>
#include <fcntl.h>
#include <stdlib.h>


typedef int Socket;

// Todas son opcionales, y si no, tenemos valores default dependiendo de donde se esta corriendo
// ./memcached -p <Port> -m <MemoryLimit> -n <NThreads>
// ./memcached --port <Port> --memory <MemoryLimit> --nthreads 8 <NThreads>


typedef struct {

  int port_number;
  unsigned long memory_limit;
  int number_threads;

} Args;


int parse_arguments(int argc, char** argv, Args* args) {

  // Seteamos a defaults
  args->port_number = 889;
  args->number_threads = sysconf(_SC_NPROCESSORS_ONLN);
  long pages = sysconf(_SC_PHYS_PAGES);
  long page_size = sysconf(_SC_PAGE_SIZE);
  unsigned long  total_ram = pages * page_size;
  args->memory_limit = total_ram;


  // Mejorar manejo de errores
  for (int i = 0 ; i < argc ; i++) {

    if (strcmp("-p", argv[i]) == 0 || strcmp("--port", argv[i]) == 0) {

      args->port_number = atoi(argv[i+1]);

    }

    else if (strcmp("-m", argv[i]) == 0 || strcmp("--memory", argv[i]) == 0) {
    
      args->memory_limit = strtoul(argv[i+1], NULL, 10);

    }

    else if (strcmp("-n", argv[i]) == 0 || strcmp("--nthreads", argv[i]) == 0) {

      args->number_threads = atoi(argv[i+1]);

    }

    else {

      // Generar error
    }
  }
  return 0; // Salio todo ok
}


Socket create_server_socket(int port) {
  
  Socket server_socket = socket(AF_INET, SOCK_STREAM, 0);

  int yes = 1;
  setsockopt(server_socket, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes);

  struct sockaddr_in server_saddr;
  server_saddr.sin_family = AF_INET;
  server_saddr.sin_port = htons(port);
  server_saddr.sin_addr.s_addr = htonl(INADDR_ANY);

  // Lo bindemos al puerto que nos pasaron
  bind(server_socket, (struct sockaddr*) &server_saddr, sizeof server_saddr);
  listen(server_socket, 100);

  return server_socket;
}

int set_memory_limit(unsigned long memory_limit){
  struct rlimit rlim = {memory_limit, memory_limit};
  setrlimit(RLIMIT_DATA, &rlim);

  return 1;
}

// Cambiar a Int
void exec_server(char* program, Socket socket, int threads) {

  char socket[100];
  char threads[100];
  sprintf(socket, "%d", socket);
  sprintf(threads, "%d", threads);

  execl(program, program, socket, threads, NULL);

}


int main(int argc, char** argv) {

  Args args;
  parse_arguments(argc, argv, &args); 
  
  printf("[Port] %d\n", args.port_number);
  printf("[Memory] %ld Bytes\n", args.memory_limit);
  printf("[Threads] %d\n", args.number_threads);

  Socket server_socket = create_server_socket(args.port_number);

  set_memory_limit(args.memory_limit);

  exec_server("./server", server_socket, args.number_threads);
 
  return 0;
}
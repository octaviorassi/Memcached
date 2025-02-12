#include "starter_utils.h"
#include <string.h>
#include <ctype.h>

#define DEFAULT_PORT 889


static void show_usage() {

  printf("Usage: ./server [options]\n");
  printf("Options:\n");
  printf("  -p, --port <port_number>      Set the port number in which the server is running.\n");
  printf("  -m, --memory  <memory_limit>  Set the memory limit of the server.\n");
  printf("  -t, --threads <num_threads>   Set the number of threads of the server.\n");
}




static int check_for_usage(int argc, char** argv) {
  
  for (int i = 0 ; i < argc ; i++)
    if (strcmp(argv[i], "--help") == 0) return 1;
  
  return 0;
}


static int is_positive_integer(char* s) {

  for (int i = 0 ; s[i] != '\0' ; i++) 
    if (! isdigit(s[i])) return 0;

  return 1;
}


void parse_arguments(int argc, char** argv, Args* args) {

  if (check_for_usage(argc, argv)) {
    show_usage();
    return;
  }

  // Seteamos a defaults
  long pages = sysconf(_SC_PHYS_PAGES);
  long page_size = sysconf(_SC_PAGE_SIZE);
  unsigned long  total_ram = pages * page_size;

  args->memory_limit = total_ram;
  args->num_threads = sysconf(_SC_NPROCESSORS_ONLN);
  args->port = DEFAULT_PORT;

  for (int i = 1 ; i < argc ; i++) {

    if (strcmp("-p", argv[i]) == 0 || strcmp("--port", argv[i]) == 0) {

      if (i + 1 < argc && is_positive_integer(argv[i+1])) {
        args->port = atoi(argv[i+1]);
        i++;
      }

      else return printf("server: error: not a valid port\n");
    }

    else if (strcmp("-m", argv[i]) == 0 || strcmp("--memory", argv[i]) == 0) {
    
      if (i + 1 < argc && is_positive_integer(argv[i+1])) {
     
        args->memory_limit = strtoul(argv[i+1], NULL, 10);
        i++;
      
      }

      else return printf("server: error: not a valid memory limit\n");
    }

    else if (strcmp("-t", argv[i]) == 0 || strcmp("--threads", argv[i]) == 0) {

      if (i + 1 < argc && is_positive_integer(argv[i+1])) {
        args->num_threads = atoi(argv[i+1]);
        i++;
      }
 
      else return printf("server: error: not a valid number of threads\n");

    }

    else return printf("server: invalid option: %s\n", argv[i]);
  }

  return 0;
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


void exec_server(char* program, Socket socket, int threads) {

  char socket_buffer[100];
  char threads_buffer[100];
  sprintf(socket_buffer, "%d", socket);
  sprintf(threads_buffer, "%d", threads);

  execl(program, program, socket_buffer, threads_buffer, NULL);
}

#include "memcached.h"
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
/*

./memcached PUERTO MEMORIA
./memcached puerto=PUERTO
./memcached MEMORIA
./memcached

 */



#define PRINT_INT(x) printf("[Debug] %d\n", x)
#define PRINT_LONG(x) printf("[Debug] %lu\n", x)

int main(int argc, char** argv) {

  int port_number = atoi(argv[1]);
  unsigned long memory_limit = strtoul(argv[2], NULL, 10);

  PRINT_INT(port_number);
  PRINT_LONG(memory_limit);
  
  int server_socket = socket(AF_INET, SOCK_STREAM, 0);
  if (server_socket < 0) quit("ERROR: SOCKET CREATION");

  struct sockaddr_in server_saddr;
  server_saddr.sin_family = AF_INET;
  server_saddr.sin_port = htons(port_number);
  server_saddr.sin_addr.s_addr = htonl(INADDR_ANY);

  // Lo bindemos al puerto que nos pasaron
  bind(server_socket, (struct sockaddr*) &server_saddr, sizeof server_saddr);
  listen(server_socket, 10);

  // Sabemos que está siendo ejecutado con sudo (si es que queria bindearse a puerto privileagiado)

  // Creamos socket TCP Stream
  
  // Cambiamos nuestros permisos - Capabilities


  // Seteamos el limite de nuetra memoria
  struct rlimit rlim = {memory_limit, memory_limit};
  setrlimit(RLIMIT_DATA, &rlim);
  
  // Hacemos un execv sobre el server, pasandole el socket si es necesario  
  execv("./server", socket_server);

  return 0;
}
#ifndef __SERVER_STARTER_UTILS_H__
#define __SERVER_STARTER_UTILS_H__

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

typedef struct {

  int port;
  unsigned long memory_limit;
  int num_threads;

} Args;

#define DEFAULT_PORT 889
#include <string.h>
#include <ctype.h>

int parse_arguments(int argc, char** argv, Args* args);

Socket create_server_socket(int port);

int set_memory_limit(unsigned long memory_limit);

void exec_server(char* program, Socket socket, int threads);


#endif // __SERVER_STARTER_UTILS_H__

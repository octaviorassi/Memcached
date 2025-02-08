#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/ip.h>
#include <sys/epoll.h>
#include <netinet/in.h>
#include <wait.h>
#include <fcntl.h>
#include <errno.h>
#include <pthread.h>



typedef struct {

  int epoll_fd;
  // La Hash

} GlobalData;

GlobalData globalData;

void working_thread(void* server_socket) {

  struct epoll_event ev;

  while(1) { // Principal hilo de ejecucion del working thread

    if (epoll_wait(globalData.epoll_fd, &ev, 1, -1) < 0)
      exit(1);
    

  }

}

void start_server(int server_socket, int n_threads) {

  epoll_fd = epoll_create1(0);

  // Cargamos el server_socket en el epoll

  pthread_t threads[n_threads];

  for (int i = 0 ; i < n_threads ; i++) 
    pthread_create(&threads[i], NULL, working_thread, &server_socket);

  for (int i = 0 ; i < n_threads ; i++)
    pthread_join(threads[i], NULL);  

}


int main(int argc, char** argv) {

  setbuf(stdout, NULL);

  // Creamos e inicializamos la hash
  int server_socket = atoi(argv[1]);
  int n_threads = atoi(argv[2]);
  return 0;
  start_server(server_socket, n_threads);
}
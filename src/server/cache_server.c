
#include "cache_server_utils.h"

#include "../cache/cache.h"

#define GREEN   "\x1b[32m"
#define RESET   "\x1b[0m"
#define RED     "\x1b[31m"

Cache global_cache;

void* working_thread(void* thread_args) {


  ThreadArgs* thread_args_casted = (ThreadArgs*) thread_args;

  int server_epoll = thread_args_casted->server_epoll;
  int server_socket = thread_args_casted->server_socket;
  int thread_number = thread_args_casted->thread_number;
  // Hash hash = thread_args_casted->hash;

  struct epoll_event event;

  while(1) { 

    printf(GREEN "[Thread %d] " RESET, thread_number); 
    printf("Wating for events\n");
    if (epoll_wait(server_epoll, &event, 1, -1) < 0)
      quit("[Error] EPOLL_WAIT");

    // Recuperamos la Data y los Eventos
    Data* event_data = (Data*) event.data.ptr;
    int events = event.events;
    
    // todo: emprolijar
    if (events & EPOLLERR || events & EPOLLHUP || events & EPOLLRDHUP) {
      printf("[Thread %d] Error en socket\n", thread_number);
      epoll_ctl(server_epoll, EPOLL_CTL_DEL, event_data->socket, NULL);
      close(event_data->socket);
    }
    

    else if (event_data->socket == server_socket) { // Alguien se quiere conectar
      
      int new_client_socket = accept(server_socket, NULL, NULL);
      printf("[Thread %d] Acepting client\n", thread_number);

      // Creamos data para el nuevo cliente
      Data* new_client_data = create_new_client_data(new_client_socket);

      // Cargamos su epoll_event
      struct epoll_event new_event;
      new_event.data.ptr = new_client_data;
      new_event.events = EPOLLIN | EPOLLRDHUP | EPOLLONESHOT;

      // Lo cargamos al epoll
      epoll_ctl(server_epoll, EPOLL_CTL_ADD, new_client_socket, &new_event);

      //? Si el socket_server no esta en ONESHOT, no lo tengo que agregar
      event.events = EPOLLIN | EPOLLONESHOT;
      epoll_ctl(server_epoll, EPOLL_CTL_MOD, server_socket, &event);
    }
    
    else { // Es un cliente escribiendo

      printf("[Thread %d] Parsing request\n", thread_number);
      parse_request(event_data);

      if(event_data->parsing_stage == PARSING_FINISHED) {
        printf("[Thread %d] Handling request\n", thread_number);
        handle_request(event_data);
        reset_client_data(event_data);
      }
      
      reconstruct_client_epoll(server_epoll, &event, event_data); 
    }
  }
}

void start_server(ServerArgs* server_args) {

  ThreadArgs thread_args;
  thread_args.server_epoll = epoll_create1(0);
  thread_args.server_socket = server_args->server_socket;
  // thread_args.hash = server_args->hash;

  Data server_data;
  server_data.socket = server_args->server_socket; // Las otras no las usamos

  struct epoll_event event;
  event.events = EPOLLIN | EPOLLONESHOT; //? Es necesario el EPOLLONESHOT
  event.data.ptr = &server_data;
  epoll_ctl(thread_args.server_epoll, EPOLL_CTL_ADD, server_data.socket, &event);

  int num_threads = server_args->num_threads;

  pthread_t threads[num_threads];
  ThreadArgs threads_args[num_threads];

  for (int i = 0 ; i < num_threads ; i++) {
    threads_args[i] = thread_args;
    threads_args[i].thread_number = i;
    
    pthread_create(&threads[i], NULL, working_thread, &threads_args[i]);
  }

  for (int i = 0 ; i < num_threads ; i++)
    pthread_join(threads[i], NULL);  
}



int main(int argc, char** argv) { // Sabemos que los argumentos son correctos

  setbuf(stdout, NULL); // Opcional

  ServerArgs server_args;
  server_args.server_socket = atoi(argv[1]);
  server_args.num_threads = atoi(argv[2]);
  global_cache = cache_create((HashFunction) kr_hash);
  server_args.cache = global_cache;

  start_server(&server_args);

  return 0;
}





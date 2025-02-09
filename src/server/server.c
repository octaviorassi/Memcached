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

#define LENGTH 4


typedef struct {

  int server_epoll;
  int server_socket;
  // Hash

} ThreadArgs;

typedef struct {

  int server_epoll;
  int server_socket;
  int num_threads;
  // Hash

} ServerArgs;

typedef enum {

  PARSING_COMMAND,
  PARSING_KEY_LEN,
  PARSING_KEY,
  PARSING_VALUE_LEN,
  PARSING_VALUE,
  PARSING_FINISHED

} ParsingStage;

typedef struct {

  int socket;

  char command;
  
  char key_size_buffer [4];
  int key_size;
  char* key;

  char value_size_buffer[4];
  int value_size;
  char* value;

  int parsing_index;
  ParsingStage parsing_stage;

} Data;


void quit(char* error) {
  perror(error);
  abort();
}

// Puede ser que no lea todo
void recv_socket(int socket, char* message_buffer, int size, Data* data) {

  int num_bytes_recv = 0;
  int bytes_recv;

  // Leo siempre y cuando haya algo para leer, y no supere el largo
  while (bytes_recv > 0 && num_bytes_recv < size) {

    bytes_recv = recv(socket, message_buffer + num_bytes_recv, size - num_bytes_recv, 0);
    num_bytes_recv += bytes_recv;
  }

  data->parsing_index += num_bytes_recv;
}

// Obligatoriamente va a tener que mandar todo
int send_socket(int socket, char* message, int size) {

  int num_bytes_send = 0; 
  int bytes_send;

  while (num_bytes_send < size) {

    bytes_send = send(socket, message + num_bytes_send, size - num_bytes_send, 0);
    if(bytes_send == -1) quit("[Error] send");
    num_bytes_send += bytes_send;

  }

  return num_bytes_send;
}


typedef enum {

  PUT       = 11,
  DEL       = 12,
  GET       = 13,
  STATS     = 21,
  OK        = 101,
  EINVALID  = 111,
  ENOTFOUND = 112,
  EBINARY   = 113,
  EBIG      = 114,
  EUNK      = 115

} Command;


void parse_request(Data* data) {

  switch (data->parsing_stage) {

    case PARSING_COMMAND:
      recv_socket(data->socket, &data->command, 1, data); 
      
      // No se termino de parsea comando
      if (data->parsing_index < 1) return; 

      // Cambio lo siguiente a hacer dependiendo del comando
      data->parsing_stage = data->command == STATS ? PARSING_FINISHED : PARSING_KEY_LEN;
      data->parsing_index = 0;

      break;

    case PARSING_KEY_LEN:
      recv_socket(data->socket, data->key_size_buffer + data->parsing_index, LENGTH - data->parsing_index, data);

      if (data->parsing_index < LENGTH) return;

      data->key_size = htonl(*(int*)(data->key_size_buffer));
      data->key = malloc(data->key_size);
      data->parsing_stage = PARSING_KEY;
      data->parsing_index = 0;
      break;

    case PARSING_KEY:
      recv_socket(data->socket, data->key + data->parsing_index, data->key_size - data->parsing_index, data);

      if (data->parsing_index < data->key_size) return;

      data->parsing_stage = data->command == PUT ? PARSING_VALUE_LEN : PARSING_FINISHED;
      data->parsing_index = 0;
      break;

    case PARSING_VALUE_LEN:
      recv_socket(data->socket, data->value_size_buffer + data->parsing_index, LENGTH - data->parsing_index, data);

      if (data->parsing_index < LENGTH) return;

      data->value_size= htonl(*(int*)(data->value_size_buffer));
      data->value = malloc(data->value_size);
      data->parsing_stage = PARSING_VALUE;
      data->parsing_index = 0;
      break;

      break;

    case PARSING_VALUE:
      
      recv_socket(data->socket, data->value + data->parsing_index, data->value_size - data->parsing_index, data);

      if (data->parsing_index < data->value_size) return;

      data->parsing_stage = PARSING_FINISHED;

      break;

    case PARSING_FINISHED: // Imposible que llege hasta aca, lanzamos un error
      quit("[Error] Switch case PARSING_FINISHED reached");
  }
}

void handle_request(Data* data) {

  char command;

  switch (data->command)
  {
  case PUT:
    printf("Me hizo un put con\n");
    printf("[KeySize] %d\n", data->key_size);
    for (int i = 0 ; i < data->key_size ; i++) printf("%u ", data->key[i]);
    printf("\n");
    printf("[ValueSize] %d\n", data->value_size);
    for (int i = 0 ; i < data->value_size ; i++) printf("%u ", data->value[i]);
    printf("\n");
    
    command = OK;
    send_socket(data->socket, &command, 1);
    break;
  
  case DEL:
    command = OK;
    send_socket(data->socket, &command, 1);
    break;
  
  case GET:
    command = ENOTFOUND;
    send_socket(data->socket, &command, 1);
    break;
  
  case STATS: // Implementar
    break;
  
  }  
}

void reset_client_data(Data* data) {

  data->parsing_index = 0;
  data->parsing_stage = PARSING_COMMAND;
  memset(data->key_size_buffer, 0, LENGTH);
  memset(data->value_size_buffer, 0, LENGTH);

}

void reconstruct_client_epoll(int fd, struct epoll_event* ev, Data* data) {
  ev->events = EPOLLIN | EPOLLRDHUP | EPOLLONESHOT;
  ev->data.ptr = data;

  if (epoll_ctl(fd, EPOLL_CTL_MOD, data->socket, ev))
    quit("[Error] epoll_ctl");
} 


Data* create_new_client_data(int client_socket) {

  Data* new_client_data = malloc(sizeof(Data));

  memset(new_client_data->key_size_buffer,0,LENGTH);
  memset(new_client_data->value_size_buffer,0,LENGTH);

  new_client_data->parsing_index = 0;
  new_client_data->parsing_stage = PARSING_COMMAND;

  new_client_data->socket = client_socket;

  return new_client_data;
}


void* working_thread(void* thread_args) {

  ThreadArgs* thread_args_casted = (ThreadArgs*) thread_args;

  int server_epoll = thread_args_casted->server_epoll;
  int server_socket = thread_args_casted->server_socket;
  // Hash hash = thread_args_casted->hash;

  struct epoll_event event;

  while(1) { 
    printf("Waitring...\n");
    if (epoll_wait(server_epoll, &event, 1, -1) < 0)
      quit("[Error] EPOLL_WAIT");

    // Recuperamos la Data y los Eventos
    Data* event_data = (Data*) event.data.ptr;
    int events = event.events;
    
      printf("[[%d]]\n", event_data->socket);
    printf("Events: %d\n", (events & EPOLLRDHUP) == EPOLLRDHUP);  
    if (events & EPOLLERR || events & EPOLLHUP || events & EPOLLRDHUP) {
      epoll_ctl(server_epoll, EPOLL_CTL_DEL, event_data->socket, NULL);
      close(event_data->socket);
    }
    
    else if (event_data->socket == server_socket) { // Alguien se quiere conectar
      printf("toy aca");
      int new_client_socket = accept(server_socket, NULL, NULL);
      printf("Aqui <<%d>>\n", new_client_socket);

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
      printf("toy aqui\n");

      parse_request(event_data);

      if(event_data->parsing_stage == PARSING_FINISHED) {
        printf("Parsing finished\n");
        handle_request(event_data);
        reset_client_data(event_data);
      }
      printf("<<%d>>\n", server_epoll);
      printf("<<%d>>\n", event_data->socket);
      
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

  for (int i = 0 ; i < num_threads ; i++) 
    pthread_create(&threads[i], NULL, working_thread, &thread_args);

  for (int i = 0 ; i < num_threads ; i++)
    pthread_join(threads[i], NULL);  
}



int main(int argc, char** argv) { // Sabemos que los argumentos son correctos

  setbuf(stdout, NULL); 

  ServerArgs server_args;
  server_args.server_socket = atoi(argv[1]);
  server_args.num_threads = atoi(argv[2]);
  // server_args.hash = hash;

  start_server(&server_args);

  return 0;
}
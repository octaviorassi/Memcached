#include "cache_server_utils.h"
#include <sys/epoll.h>
#include <stdio.h>

int operations = 0;

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
      data->key = dynalloc(data->key_size); //!! DYNALLOC
      data->parsing_stage = PARSING_KEY;
      data->parsing_index = 0;
      
      parse_request(data);
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
      data->value = dynalloc(data->value_size);
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
      break;
  }

  if (data->parsing_stage != PARSING_FINISHED) parse_request(data); //? Ver si esto funciona bien
}


extern Cache global_cache; //!! ELIMINAR

void handle_request(Data* data) {

  operations++;


  char command;

  switch (data->command) {
    case PUT:
      printf("---------------------\n");
      printf("PUT Operation %d\n",operations);
      printf("[KeySize] %d\n", data->key_size);
      for (int i = 0 ; i < data->key_size ; i++) printf("%c ", data->key[i]);
      printf("\n");
      printf("[ValueSize] %d\n", data->value_size);
      for (int i = 0 ; i < data->value_size ; i++) printf("%c ", data->value[i]);
      printf("\n");


      int result = cache_put(data->key, data->key_size, data->value, data->value_size, global_cache);
      
      if (result == 0) {
        command = OKAY;
        send_socket(data->socket, &command, 1);
      } 

      else {
        //TODO COMPLETAR
      }

      break;
  
  case DEL:
      
      printf("---------------------\n");
      printf("DEL Operation %d\n", operations);
      printf("[KeySize] %d\n", data->key_size);
      for (int i = 0 ; i < data->key_size ; i++) printf("%c ", data->key[i]);
      printf("\n");


      result = cache_delete(data->key, data->key_size, global_cache);
      
      command = result ? OKAY : ENOTFOUND;
      send_socket(data->socket, &command, 1);
      
      break;
  
  case GET:
      printf("---------------------\n");
      printf("GET Operation %d\n", operations);
      printf("[KeySize] %d\n", data->key_size);
      for (int i = 0 ; i < data->key_size ; i++) printf("%c ", data->key[i]);
      printf("\n");

      LookupResult l_result = cache_get(data->key, data->key_size, global_cache);

      if (lookup_result_is_ok(l_result)) {
        
        //todo: el cache_get, deberia devolver el size tambien
        //todo: ver porque despues de un update, tira notfound

        // Tengo que poner <<OK,LV,V>>
        //!! Esto no esta bueno ver como hacer
        char message[5 + data->value_size];
        message[0] = OKAY;
        memcpy(message + 1, data->value_size_buffer, LENGTH);
        memcpy(message + 5, data->value, data->value_size);

        send_socket(data->socket, message, 5 + data->value_size);
      }

      else {

        command = ENOTFOUND;
        send_socket(data->socket, &command, 1);
      }

      
      break;
  
  case STATS: // Implementar
    break;
  
  }  
}

void reset_client_data(Data* data) {

  data->parsing_index = 0;
  data->parsing_stage = PARSING_COMMAND;
  // memset(data->key_size_buffer, 0, LENGTH);
  // memset(data->value_size_buffer, 0, LENGTH);

}

void reconstruct_client_epoll(int fd, struct epoll_event* ev, Data* data) {
  
  ev->events = EPOLLIN | EPOLLRDHUP | EPOLLONESHOT;
  ev->data.ptr = data;

  if (epoll_ctl(fd, EPOLL_CTL_MOD, data->socket, ev) == -1)
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


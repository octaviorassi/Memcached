#include <sys/epoll.h>
#include <stdio.h>
#include "cache_server_utils.h"
#include "../cache/cache.h" // ! No se si esta bien incluir esto o puede generar problemas de dependencias.

// ? Problema: yo quiero que solo vea las funciones que expone la cache, no quiero que cache_utils pueda ver las funciones que expone cache_stats a cache. 

int operations = 0;

void quit(char* error) {
  // ! perror(msg) deberia contener en msg una explicacion del valor de error setteado en errno. aca faltaria haber setteado el errno?
  perror(error);
  abort();
}


ssize_t recv_socket(int socket, char* message_buffer, int size, Data* data) {

  ssize_t total_bytes_received = 0;
  ssize_t bytes_received;

  while (bytes_received > 0 && total_bytes_received < size) {

    bytes_received = recv(socket, message_buffer + total_bytes_received,
                          size - bytes_received, 0);

    total_bytes_received+= bytes_received;
  }

  data->parsing_index += total_bytes_received;

  return total_bytes_received;
}


ssize_t send_socket(int socket, char* message, int size) {

  ssize_t total_bytes_sent = 0; 
  ssize_t bytes_sent;

  while (total_bytes_sent < size) {

    bytes_sent = send(socket, message + total_bytes_sent,
                      size - total_bytes_sent, 0);

    // ! Deberia retornar -1 aca tambien no? Y settear errno si voy a invocar a quit()
    if (bytes_sent == -1)
      quit("[Error] send");

    total_bytes_sent += bytes_sent;

  }

  return total_bytes_sent;

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

      data->value_size = htonl(*(int*)(data->value_size_buffer));
      data->value = dynalloc(data->value_size);
      data->parsing_stage = PARSING_VALUE;
      data->parsing_index = 0;
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


static void serialize_stats_report(const StatsReport* report, char* buffer) {
  
  // Casteamos al buffer de chars a buffer de Counter para insertar mas facil.
  Counter* buffer_ptr = (Counter*) buffer;

  // Ahora insertamos en el buffer en el orden preestablecido, convirtiendo al network order.
  buffer_ptr[0] = ntohl(report->put);
  buffer_ptr[1] = ntohl(report->get);
  buffer_ptr[2] = ntohl(report->del);
  buffer_ptr[3] = ntohl(report->key);
  buffer_ptr[4] = ntohl(report->evict);

}


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

      if (result == 0) command = OKAY;
      else command = ENOTFOUND;

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
        
        printf("SIZEEE: %ld\n", l_result.size);
        char command = OKAY;
        char length_buffer[LENGTH];
        size_t size = ntohl(l_result.size);
        memcpy(length_buffer, &size, LENGTH);

        send_socket(data->socket, &command, 1);
        send_socket(data->socket, length_buffer, LENGTH);
        send_socket(data->socket, l_result.ptr, l_result.size);
      }

      else if (lookup_result_is_error(l_result)){

        printf("ERROR LOOKUP\n");
        command = ENOTFOUND;
        send_socket(data->socket, &command, 1);
      }

      else {
        printf("MISS LOOKUP\n");
        command = ENOTFOUND;
        send_socket(data->socket, &command, 1);
      }
      
      break;
  
  case STATS: 
    
    // Obtengo el reporte de estadisticas.
    StatsReport report = cache_report(global_cache);

    // Creo un buffer donde cargar cada counter
    size_t buffer_size = sizeof(Counter) * STATS_COUNT;
    char report_buffer[buffer_size];

    // Serializamos el reporte de estadisticas en el buffer y lo enviamos
    serialize_stats_report(&report, report_buffer);
    send_socket(data->socket, report_buffer, buffer_size);

    break;
  
  }  
}




void reset_client_data(Data* data) {

  data->parsing_index = 0;
  data->parsing_stage = PARSING_COMMAND;
  // memset(data->key_size_buffer, 0, LENGTH);
  // memset(data->value_size_buffer, 0, LENGTH);

}


int reconstruct_client_epoll(int epoll_fd, struct epoll_event* ev, Data* data) {
  
  ev->events = EPOLLIN | EPOLLRDHUP | EPOLLONESHOT;
  ev->data.ptr = data;

  int epoll_status = epoll_ctl(epoll_fd, EPOLL_CTL_MOD, data->socket, ev);

  // ! Aca habia un quit antes, pero me parecio mas apropiado retornar el epoll_status en vez de quittear directamente.
  return epoll_status;

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


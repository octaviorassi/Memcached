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


ssize_t recv_socket(int socket, char* message_buffer, int size, ClientData* cdata) {

  ssize_t total_bytes_received = 0;
  ssize_t bytes_received;

  while (bytes_received > 0 && total_bytes_received < size) {

    bytes_received = recv(socket, message_buffer + total_bytes_received,
                          size - total_bytes_received, 0);

    total_bytes_received+= bytes_received;
  }

  cdata->parsing_index += total_bytes_received;

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


void parse_request(ClientData* cdata) {

  switch (cdata->parsing_stage) {

    case PARSING_COMMAND:
      
      recv_socket(cdata->socket, &cdata->command, 1, cdata); 
      
      // No se termino de parsea comando
      if (cdata->parsing_index < 1) return; 

      // Cambio lo siguiente a hacer dependiendo del comando
      cdata->parsing_stage = cdata->command == STATS ?
                                   PARSING_FINISHED : PARSING_KEY_LEN;
      cdata->parsing_index = 0;
      break;

    case PARSING_KEY_LEN:

      recv_socket(cdata->socket, cdata->key_size_buffer + cdata->parsing_index, LENGTH - cdata->parsing_index, cdata);

      if (cdata->parsing_index < LENGTH) return;

      cdata->key_size = htonl(*(int*)(cdata->key_size_buffer));
      cdata->key = dynalloc(cdata->key_size); //!! DYNALLOC
      cdata->parsing_stage = PARSING_KEY;
      cdata->parsing_index = 0;
      
      parse_request(cdata);
      break;

    case PARSING_KEY:

      recv_socket(cdata->socket, cdata->key + cdata->parsing_index, cdata->key_size - cdata->parsing_index, cdata);

      if (cdata->parsing_index < cdata->key_size) return;

      cdata->parsing_stage = cdata->command == PUT ? PARSING_VALUE_LEN : PARSING_FINISHED;
      cdata->parsing_index = 0;
      break;

    case PARSING_VALUE_LEN:

      recv_socket(cdata->socket, cdata->value_size_buffer + cdata->parsing_index, LENGTH - cdata->parsing_index, cdata);

      if (cdata->parsing_index < LENGTH) return;

      cdata->value_size = htonl(*(int*)(cdata->value_size_buffer));
      cdata->value = dynalloc(cdata->value_size);
      cdata->parsing_stage = PARSING_VALUE;
      cdata->parsing_index = 0;
      break;

    case PARSING_VALUE:
      
      recv_socket(cdata->socket, cdata->value + cdata->parsing_index, cdata->value_size - cdata->parsing_index, cdata);

      if (cdata->parsing_index < cdata->value_size) return;

      cdata->parsing_stage = PARSING_FINISHED;

      break;

    case PARSING_FINISHED: // Imposible que llege hasta aca, lanzamos un error
      quit("[Error] Switch case PARSING_FINISHED reached");
      break;
  }

  if (cdata->parsing_stage != PARSING_FINISHED) parse_request(cdata); //? Ver si esto funciona bien
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


void handle_request(ClientData* cdata) {

  operations++;

  char command;

  switch (cdata->command) {
    case PUT:
      printf("---------------------\n");
      printf("PUT Operation %d\n",operations);
      printf("[KeySize] %d\n", cdata->key_size);
      for (int i = 0 ; i < cdata->key_size ; i++) printf("%c ", cdata->key[i]);
      printf("\n");
      printf("[ValueSize] %d\n", cdata->value_size);
      for (int i = 0 ; i < cdata->value_size ; i++) printf("%c ", cdata->value[i]);
      printf("\n");


      int result = cache_put(cdata->key, cdata->key_size, cdata->value, cdata->value_size, global_cache);
      
      if (result == 0) {
        command = OKAY;
        send_socket(cdata->socket, &command, 1);
      } 

      else {
        //TODO COMPLETAR
      }

      break;
  
  case DEL:
      
      printf("---------------------\n");
      printf("DEL Operation %d\n", operations);
      printf("[KeySize] %d\n", cdata->key_size);
      for (int i = 0 ; i < cdata->key_size ; i++) printf("%c ", cdata->key[i]);
      printf("\n");


      result = cache_delete(cdata->key, cdata->key_size, global_cache);

      if (result == 0) command = OKAY;
      else command = ENOTFOUND;

      send_socket(cdata->socket, &command, 1);
      
      break;
  
  case GET:
  
      printf("---------------------\n");
      printf("GET Operation %d\n", operations);
      printf("[KeySize] %d\n", cdata->key_size);
      for (int i = 0 ; i < cdata->key_size ; i++) printf("%c ", cdata->key[i]);
      printf("\n");

      LookupResult l_result = cache_get(cdata->key, cdata->key_size, global_cache);

      if (lookup_result_is_ok(l_result)) {
        
        printf("SIZEEE: %ld\n", l_result.size);
        char command = OKAY;
        char length_buffer[LENGTH];
        size_t size = ntohl(l_result.size);
        memcpy(length_buffer, &size, LENGTH);

        send_socket(cdata->socket, &command, 1);
        send_socket(cdata->socket, length_buffer, LENGTH);
        send_socket(cdata->socket, l_result.ptr, l_result.size);
      }

      else if (lookup_result_is_error(l_result)){

        printf("ERROR LOOKUP\n");
        command = ENOTFOUND;
        send_socket(cdata->socket, &command, 1);
      }

      else {
        printf("MISS LOOKUP\n");
        command = ENOTFOUND;
        send_socket(cdata->socket, &command, 1);
      }
      
      break;
  
  case STATS: 
    
    // Obtengo el reporte de estadisticas.
    StatsReport report = cache_report(global_cache);

    // Creo un buffer donde cargar el mensaje y lo llenamos
    size_t buffer_size = sizeof(char) * STATS_MESSAGE_LENGTH;
    char report_buffer[buffer_size];
    char command = STATS;

    int report_len = stats_report_stringify(report, report_buffer);

    char length_buffer[LENGTH];
    size_t size = ntohl(report_len);
    memcpy(length_buffer, &size, LENGTH);

    // Serializamos el reporte de estadisticas en el buffer y lo enviamos
    send_socket(cdata->socket, &command, 1);                // Mando STATS 
    send_socket(cdata->socket, length_buffer, LENGTH);      // Prefijo longitud
    send_socket(cdata->socket, report_buffer, report_len);  // String del report

    break;
  
  }  
}

void reset_client_data(ClientData* cdata) {

  cdata->parsing_index = 0;
  cdata->parsing_stage = PARSING_COMMAND;
  // memset(data->key_size_buffer, 0, LENGTH);
  // memset(data->value_size_buffer, 0, LENGTH);

}


int reconstruct_client_epoll(int epoll_fd, struct epoll_event* ev, ClientData* cdata) {
  
  ev->events = EPOLLIN | EPOLLRDHUP | EPOLLONESHOT;
  ev->data.ptr = cdata;

  return epoll_ctl(epoll_fd, EPOLL_CTL_MOD, cdata->socket, ev);

} 

int construct_new_client_epoll(int epoll_fd, ClientData* cdata) {

  struct epoll_event ev;

  ev.events   = EPOLLIN | EPOLLRDHUP | EPOLLONESHOT;
  ev.data.ptr = cdata;

  return epoll_ctl(epoll_fd, EPOLL_CTL_ADD, cdata->socket, &ev);

}


ClientData* create_new_client_data(int client_socket) {

  ClientData* new_cdata = malloc(sizeof(ClientData));

  memset(new_cdata->key_size_buffer,0,LENGTH);
  memset(new_cdata->value_size_buffer,0,LENGTH);

  new_cdata->parsing_index = 0;
  new_cdata->parsing_stage = PARSING_COMMAND;

  new_cdata->socket = client_socket;

  return new_cdata;
}


#include <sys/epoll.h>
#include <stdio.h>
#include "cache_server_utils.h"
#include "../cache/cache.h" // ! No se si esta bien incluir esto o puede generar problemas de dependencias.

// ? Problema: yo quiero que solo vea las funciones que expone la cache, no quiero que cache_utils pueda ver las funciones que expone cache_stats a cache. 

int operations = 0;


ssize_t recv_socket(int socket, char* message_buffer, int size, ClientData* cdata) {

  ssize_t total_bytes_received = 0;
  ssize_t bytes_received;

  do {

    bytes_received = recv(socket, message_buffer + total_bytes_received,
                          size - total_bytes_received, 0);
    
    total_bytes_received += bytes_received;

  } while (bytes_received > 0 && total_bytes_received < size);

  if (bytes_received < 0) {
    perror("Error: failed to call recv bytes.");;
    return -1;
  }

  cdata->parsing_index += total_bytes_received;

  return total_bytes_received;
}


ssize_t send_socket(int socket, char* message, int size) {

  ssize_t total_bytes_sent = 0; 
  ssize_t bytes_sent;


  do {

    bytes_sent = send(socket, message + total_bytes_sent,
                      size - total_bytes_sent, 0);

    total_bytes_sent += bytes_sent;

  } while (bytes_sent > 0 && total_bytes_sent < size);

  if (bytes_sent < 0) {
    perror("Error: failed to call send bytes.");;
    return -1;
  }

  return total_bytes_sent;

}

// ! cambiar la documentacion para reflear el nuevo return type
int parse_request(ClientData* cdata) {

  switch (cdata->parsing_stage) {

    case PARSING_COMMAND:
      
      if (recv_socket(cdata->socket, &cdata->command, 1, cdata) < 0) return -1; 
      
      // No se termino de parsear el comando
      if (cdata->parsing_index < 1) return 0; 

      // Cambio lo siguiente a hacer dependiendo del comando
      cdata->parsing_stage = cdata->command == STATS ?
                             PARSING_FINISHED : PARSING_KEY_LEN;
      cdata->parsing_index = 0;
      break;

    case PARSING_KEY_LEN:

      if (recv_socket(cdata->socket, cdata->key_size_buffer +
                      cdata->parsing_index, LENGTH - cdata->parsing_index, cdata) < 0) return -1;

      if (cdata->parsing_index < LENGTH) return 0;

      cdata->key_size = htonl(*(int*)(cdata->key_size_buffer));
      cdata->key = dynalloc(cdata->key_size);
      cdata->parsing_stage = PARSING_KEY;
      cdata->parsing_index = 0;
      
      return parse_request(cdata);
      break;

    case PARSING_KEY:

      if (recv_socket(cdata->socket, cdata->key + cdata->parsing_index, cdata->key_size - cdata->parsing_index, cdata) < 0) return -1;

      if (cdata->parsing_index < cdata->key_size) return 0;

      cdata->parsing_stage = cdata->command == PUT ? PARSING_VALUE_LEN : PARSING_FINISHED;
      cdata->parsing_index = 0;
      break;

    case PARSING_VALUE_LEN:

      if (recv_socket(cdata->socket,
                      cdata->value_size_buffer + cdata->parsing_index,
                      LENGTH - cdata->parsing_index, cdata) < 0) return -1;

      if (cdata->parsing_index < LENGTH) return 0;

      cdata->value_size = htonl(*(int*)(cdata->value_size_buffer));
      cdata->value = dynalloc(cdata->value_size);
      cdata->parsing_stage = PARSING_VALUE;
      cdata->parsing_index = 0;
      break;

    case PARSING_VALUE:
      
      if (recv_socket(cdata->socket, cdata->value + cdata->parsing_index,
                      cdata->value_size - cdata->parsing_index, cdata) < 0) return -1;

      if (cdata->parsing_index < cdata->value_size) return 0;

      cdata->parsing_stage = PARSING_FINISHED;

      break;

    case PARSING_FINISHED: // Imposible que llege hasta aca, lanzamos un error
      quit("Error: switch case PARSING_FINISHED reached");
      break;
  }

  if (cdata->parsing_stage != PARSING_FINISHED) return parse_request(cdata); //? Ver si esto funciona bien
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
  buffer_ptr[5] = ntohl(report->allocated_memory);
}


int handle_request(ClientData* cdata) {

  operations++;

  char command;

  switch (cdata->command) {
    case PUT:

      int result = cache_put(cdata->key, cdata->key_size, cdata->value, cdata->value_size, global_cache);
      
      if (result == 0) {
        command = OKAY;
        if (send_socket(cdata->socket, &command, 1) < 0) return -1;
      } 

      else {
        //TODO COMPLETAR
      }

      break;
  
  case DEL:
      
      result = cache_delete(cdata->key, cdata->key_size, global_cache);

      if (result == 0) command = OKAY;
      else             command = ENOTFOUND;

      if (send_socket(cdata->socket, &command, 1) < 0) return -1;
      
      break;
  
  case GET:
  
      LookupResult l_result = cache_get(cdata->key, cdata->key_size, global_cache);

      if (lookup_result_is_ok(l_result)) {
        
        char command = OKAY;
        char length_buffer[LENGTH];
        size_t size = ntohl(l_result.size);
        memcpy(length_buffer, &size, LENGTH);

        // ! ESTO ROMPE LA INTERFAZ DE LOOKUPRESULT :(
        if (send_socket(cdata->socket, &command, 1) < 0) return -1;
        if (send_socket(cdata->socket, length_buffer, LENGTH) < 0) return -1;
        if (send_socket(cdata->socket, l_result.ptr, l_result.size) < 0)
          return -1;
      }

      else if (lookup_result_is_error(l_result)) {

        printf("ERROR LOOKUP\n");
        command = ENOTFOUND;
        if (send_socket(cdata->socket, &command, 1) < 0) return -1;
      }

      else {
        printf("MISS LOOKUP\n");
        command = ENOTFOUND;
        if (send_socket(cdata->socket, &command, 1) < 0) return -1;
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
    if (send_socket(cdata->socket, report_buffer, buffer_size) < 0) return -1;

    break;
  
  }

  return 0;

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

void delete_client_data(ClientData* cdata) {

  if (cdata == NULL)
    return;

  if (cdata->socket > 0)
    close(cdata->socket);

  free(cdata);

}

void drop_client(int epoll_fd, ClientData* cdata) {
  epoll_ctl(epoll_fd, EPOLL_CTL_DEL, cdata->socket, NULL);
  delete_client_data(cdata);
}


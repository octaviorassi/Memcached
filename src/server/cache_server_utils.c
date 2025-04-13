#include <sys/epoll.h>
#include <stdio.h>
#include "cache_server_utils.h"
#include "cache_server_models.h"
#include "../cache/cache.h" // ! No se si esta bien incluir esto o puede generar problemas de dependencias.

// ? Problema: yo quiero que solo vea las funciones que expone la cache, no quiero que cache_utils pueda ver las funciones que expone cache_stats a cache. 
#define TRASH_BUFFER_SIZE 100


ssize_t clean_socket(ClientData* client, int size) {

  int socket = client->socket;

  ssize_t total_bytes_received = 0;
  ssize_t bytes_received;

  char buffer[TRASH_BUFFER_SIZE];

  do {

    bytes_received = recv(socket, buffer, TRASH_BUFFER_SIZE, 0);

    total_bytes_received += bytes_received;

  } while (bytes_received > 0 && total_bytes_received < size);

  if (bytes_received < 0) {
    perror("Error: failed to call recv bytes.");;
    return -1;
  }

  client->parsing_index += total_bytes_received;

  return total_bytes_received;
}



ssize_t recv_client(ClientData* client, char* message_buffer, int size) {

  int socket = client->socket;

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

  client->parsing_index += total_bytes_received;

  return total_bytes_received;
}


ssize_t send_client(ClientData* client, char* message, int size) {

  int socket = client->socket;

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


static int command_is_valid(char cmd) {
  return cmd == PUT || cmd == GET || cmd == DEL || cmd == STATS;
}

int parse_request(ClientData* cdata, Cache cache) {

  switch (cdata->parsing_stage) {

    case PARSING_COMMAND:
      
      if (recv_client(cdata, &cdata->command, 1) < 0) return -1; 
      
      // No se termino de parsear el comando
      if (cdata->parsing_index < 1) return 0; 

      // Si el comando no es valido, devolvemos -1 y se le cierra la conexion
      if (!command_is_valid(cdata->command)) return -1;

      // Si no, determinamos el estado de parseo dependiendo de si es STATS o no
      cdata->parsing_stage = cdata->command == STATS ?
                             PARSING_FINISHED : PARSING_KEY_LEN;
      cdata->parsing_index = 0;

      break;

    case PARSING_KEY_LEN:

      if (recv_client(cdata,
                      cdata->key_size_buffer + cdata->parsing_index,
                      LENGTH_PREFIX_SIZE - cdata->parsing_index) < 0) return -1;

      if (cdata->parsing_index < LENGTH_PREFIX_SIZE) return 0;

      cdata->key_size = htonl(*(int*)(cdata->key_size_buffer));
      
      cdata->key = dynalloc(cdata->key_size, cache); 
      if (cdata->key == NULL) {
        // Si falla la asignacion de memoria, marcamos el comando como EBIG.
        cdata->command = EBIG;
        cdata->cleaning = 1;
      }

      cdata->parsing_stage = PARSING_KEY;
      cdata->parsing_index = 0;
      
      break;

    case PARSING_KEY:
     
      if (cdata->cleaning){
        if (clean_socket(cdata, cdata->key_size - cdata->parsing_index) < 0) return -1;
      } 

      else if (recv_client(cdata, cdata->key + cdata->parsing_index,
                           cdata->key_size - cdata->parsing_index) < 0) return -1;



      if (cdata->parsing_index < cdata->key_size) return 0;

      cdata->parsing_stage = cdata->command == PUT ?
                             PARSING_VALUE_LEN : PARSING_FINISHED;
      cdata->parsing_index = 0;

      break;

    case PARSING_VALUE_LEN:

      if (recv_client(cdata,
                      cdata->value_size_buffer + cdata->parsing_index,
                      LENGTH_PREFIX_SIZE - cdata->parsing_index) < 0) return -1;

      if (cdata->parsing_index < LENGTH_PREFIX_SIZE) return 0;

      cdata->value_size = htonl(*(int*)(cdata->value_size_buffer));

      cdata->value = dynalloc(cdata->value_size, cache);
      if (cdata->value == NULL) {
        // Analogo al caso de fallar en key, pero libero la memoria
        free(cdata->key);
        cdata->command = EBIG;
        cdata->cleaning = 1;
      }

      cdata->parsing_stage = PARSING_VALUE;
      cdata->parsing_index = 0;
      break;

    case PARSING_VALUE:
      
      if (cdata->cleaning){
        if (clean_socket(cdata, cdata->key_size - cdata->parsing_index) < 0) return -1;
      } 

      else if (recv_client(cdata, cdata->value + cdata->parsing_index,
                      cdata->value_size - cdata->parsing_index) < 0) return -1;

      if (cdata->parsing_index < cdata->value_size) return 0;

      cdata->parsing_stage = PARSING_FINISHED;

      break;

    case PARSING_FINISHED: // Imposible que llege hasta aca, lanzamos un error
      quit("Error: switch case PARSING_FINISHED reached");
      break;
  }

  if (cdata->parsing_stage != PARSING_FINISHED)
    return parse_request(cdata, cache);

}

int handle_request(ClientData* cdata, Cache cache) {

   // En todo momento, si hay un problema con el socket devolvemos -1. Del lado del server, esto hace que se droppee al cliente.

  char command;

  switch (cdata->command) {

    case PUT:

      int put_status = cache_put(cdata->key, cdata->key_size, cdata->value, cdata->value_size, cache);
      
      // Si el put fue exitoso, respondemos con OK, si no, con EUNK
      command = put_status == 0 ? OKAY : EUNK;

      if (send_client(cdata, &command, 1) < 0) return -1;

      break;
  
    case DEL:
      
      int del_status = cache_delete(cdata->key, cdata->key_size, cache);

      command = del_status == 0 ? OKAY :
               (del_status == 1 ? ENOTFOUND : EUNK);

      if (send_client(cdata, &command, 1) < 0) return -1;
      
      break;
  
    case GET:
  
      LookupResult lr = cache_get(cdata->key, cdata->key_size, cache);

      if (lookup_result_is_ok(lr)) {
        
        char command = OKAY;
        char length_prefix_buffer[LENGTH_PREFIX_SIZE];
        size_t size = ntohl(lookup_result_get_size(lr));
        memcpy(length_prefix_buffer, &size, LENGTH_PREFIX_SIZE);

        if (send_client(cdata, &command, 1) < 0) return -1;
        if (send_client(cdata, length_prefix_buffer, LENGTH_PREFIX_SIZE) < 0) return -1;
        if (send_client(cdata,
                        lookup_result_get_value(lr),
                        lookup_result_get_size(lr)) < 0) return -1;
      }

      else {
        command = ENOTFOUND;
        if (send_client(cdata, &command, 1) < 0) return -1;
      }
      
      break;
  
    case STATS: {
      
      // Obtengo el reporte de estadisticas.
      StatsReport report = cache_report(cache);

      // Creo un buffer donde cargar el mensaje y lo llenamos
      size_t buffer_size = sizeof(char) * STATS_MESSAGE_LENGTH;
      char report_buffer[buffer_size];
      char command = OKAY;
    
      int report_len = stats_report_stringify(report, report_buffer);

      char length_prefix_buffer[LENGTH_PREFIX_SIZE];
      size_t size = ntohl(report_len);
      memcpy(length_prefix_buffer, &size, LENGTH_PREFIX_SIZE);

      // Enviamos el comando el mensaje
      send_client(cdata, &command, 1);              // Mando STATS 
      send_client(cdata, length_prefix_buffer, LENGTH_PREFIX_SIZE);    // Prefijo longitud
      send_client(cdata, report_buffer, report_len);  // String del report

      break;
    }

    case EBIG:

      command = EBIG;
      if (send_client(cdata, &command, 1) < 0) return -1;

      break;
  
  }  

  return 0;

}


void reset_client_data(ClientData* cdata) {

  cdata->parsing_index = 0;
  cdata->parsing_stage = PARSING_COMMAND;
  cdata->cleaning = 0;

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


ClientData* create_new_client_data(int client_socket, Cache cache) {

  ClientData* new_cdata = dynalloc(sizeof(ClientData), cache);

  memset(new_cdata->key_size_buffer,0,LENGTH_PREFIX_SIZE);
  memset(new_cdata->value_size_buffer,0,LENGTH_PREFIX_SIZE);

  new_cdata->parsing_index = 0;
  new_cdata->parsing_stage = PARSING_COMMAND;

  new_cdata->socket = client_socket;

  new_cdata->cleaning = 0;

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

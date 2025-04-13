#ifndef __CACHE_SERVER_UTILS_H__
#define __CACHE_SERVER_UTILS_H__

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

#include "../dynalloc/dynalloc.h"
#include "../cache/cache.h"
#include "cache_server_models.h"
#include "../helpers/quit.h"

/**
 *  @brief Lee hasta `size` bytes del `client` en `message_buffer` actualizando acordemente el indice de parseo del `ClientData`.
 * 
 *  @param[out] client Estructura de datos del cliente. 
 *  @param message_buffer Buffer de lectura.
 *  @param size Cantidad maxima de bytes a leer.
 *  
 *  @return La cantidad de bytes efectivamente leidos, o -1 si se produjo un error.
 */
ssize_t recv_client(ClientData* client, char* message_buffer, int size);



/**
 *  @brief Consume hasta `size` bytes del `client` actualizando acordemente el indice de parseo del `ClientData`. No guarda la informacion.
 * 
 *  @param[out] client Estructura de datos del cliente. 
 *  @param size Cantidad maxima de bytes a consumir.
 *  
 *  @return La cantidad de bytes efectivamente consumidos, o -1 si se produjo un error.
 */
ssize_t clean_socket(ClientData* client, int size);



/**
 *  @brief Escribe `size` bytes del mensaje `message` al `client` objetivo.
 * 
 *  @param client Estructura del cliente a escribir.
 *  @param message Buffer con el contenido del mensaje.
 *  @param size Cantidad de bytes a escribir.
 * 
 *  @return La cantidad de bytes enviados, que es -1 si se produjo un error.
 */
ssize_t send_client(ClientData* client, char* message, int size);

/**
 *  @brief Parsea el mensaje proveniente del cliente asociado al `data` de acuerdo a su parsing stage. Este parseo siempre es total; en caso de recibirse un mensaje incompleto, la informacion del cliente se actualiza y se vuelve a invocar la funcion recursivamente hasta que se complete el mensaje o se produza un error.
 * 
 *  @param[out] cdata Puntero a la estructura con la informacion del cliente que es actualizada de acuerdo a la informacion parseada.
 *  @param[in] cache El puntero a la cache asociada al pedido a parsear.
 *
 *  @return 0 si parseo correctamente, -1 si fracaso al recibir bytes del socket.
 */
int parse_request(ClientData* cdata, Cache cache);


/**
 *  @brief Ejecuta el comando cargado en la estructura con informacion del cliente apuntada por `data` en su campo `command`.
 * 
 *  @param cdata Puntero a la estructura con informacion del cliente.
 *  @param[in] cache El puntero a la cache asociada al pedido a parsear.
 *
 *  @return 0 si la ejecucion del pedido es exitosa, -1 si se produjo un error al intentar enviar la respuesta.
 */
int handle_request(ClientData* cdata, Cache cache);


/**
 *  @brief Restablece la estructura de informacion del cliente apuntada por `data`, inicializando a 0 los valores asociados al stage de parseo.
 * 
 *  @param[out] data Puntero a la estructura de informacion del cliente a restablecer.
 */
void reset_client_data(ClientData* data);

/**
 *  @brief Reconstruye el epoll_event asociado al cliente cuya informacion se almacena en la estructura apuntada por `data`, y carga el evento en la instancia de epoll asociada a `epoll_fd` para su control. 
 * 
 *  @param epoll_fd File descriptor de la instancia de epoll objetivo.
 *  @param[out] epoll_event Puntero a la estructura del epoll_event a reconstruir.
 *  @param data Puntero a la estructura de informacion del cliente a controlar.
 * 
 *  @return 0 si la manipulacion de la instancia de epoll es exitosa, -1 si no.
 */
int reconstruct_client_epoll(int epoll_fd, struct epoll_event* ev, ClientData* data);


/**
 *  @brief Reconstruye el epoll_event asociado al cliente cuya informacion se almacena en la estructura apuntada por `data`, y carga el evento en la instancia de epoll asociada a `epoll_fd` para su control. 
 * 
 *  @param epoll_fd File descriptor de la instancia de epoll objetivo.
 *  @param data Puntero a la estructura de informacion del cliente a controlar.
 * 
 *  @return 0 si la manipulacion de la instancia de epoll es exitosa, -1 si no.
 */
int construct_new_client_epoll(int epoll_fd, ClientData* cdata);


/**
 *  @brief Crea e inicializa una nueva estructura de informacion del cliente con el socket de cliente establecido a `client_socket`.
 *  
 *  @param client_socket File descriptor del socket del cliente.
 *  @param cache Puntero a la cache con la que se comunicara el nuevo cliente.
 * 
 *  @return Un puntero a la estructura creada e inicializada.
 */
ClientData* create_new_client_data(int client_socket, Cache cache);


/**
 *  @brief Destruye la estructura de informacion del cliente objetivo.
 *  
 *  @param cdata El puntero a la estructura a destruir. 
 * 
 */
void delete_client_data(ClientData* cdata);

/**
 * @brief Desconecta a un cliente de la instancia epoll y destruye su estructura de cliente asociada.
 *
 * @param epoll_fd File descriptor de la instancia de epoll donde se monitoreaba al cliente.
 * @param[out] cdata Puntero a la estructura del cliente a eliminar. 
 *
 */
void drop_client(int epoll_fd, ClientData* cdata);


#endif // __CACHE_SERVER_UTILS_H__
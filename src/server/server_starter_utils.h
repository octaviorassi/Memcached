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
#include <string.h>
#include <ctype.h>


typedef struct {

  int port;
  unsigned long memory_limit;
  int num_threads;

} Args;

#define DEFAULT_PORT 889

/**
 *  @brief Recibe el `argc` y `argv` de `main` y parsea sus contenidos cargandolos en `*args`. Reconoce las banderas definidas en main y establece valores defaults en caso de omitirse alguna.
 * 
 *  @param[in] argc Cantidad de argumentos leidos al ejecutarse el programa.
 *  @param[in] argv Array de argumentos leidos.
 *  @param[out] args Puntero a la estructura Args donde cargaremos los resultados del parseo.
 * 
 *  @return 0 en caso de exito, 1 si el usuario solicito ayuda para la ejecucion.
 */
int parse_arguments(int argc, char** argv, Args* args);


/**
 *  @brief Inicializa el socket donde el servidor escuchara las nuevas conexiones. El socket se bindea al `port` TCP pasado por argumento, con `domain` `AF_INET` y tipo stream, y se coloca en modo escucha.
 * 
 *  @param[in] port Numero de puerto TCP al que se bindeara el socket.
 * 
 *  @return El file descriptor asociado al socket creado.
 */
int create_server_socket(int port);


/**
 *  @brief Define el tope de memoria disponible para el proceso llamante, setteando tanto el soft como el hard limit a `memory_limit`, medido en bytes.
 * 
 *  @param memory_limit Cantidad de memoria maxima medida en bytes. 
 */
void set_memory_limit(unsigned long memory_limit);


/**
 *  @brief Ejecuta el servidor cuyo ejecutable se encuentra en el path `program`, pasandole como argumentos el file descriptor de su socket asociado `socket` y la cantidad de threads `threads`.
 * 
 *  @param program El path al ejecutable del servidor memcached.
 *  @param socket  El file descriptor del socket asociado al servidor.
 *  @param threads La cantidad de threads con las que se lanzara el server.
 */
void exec_server(char* program, int socket, int threads);


#endif // __SERVER_STARTER_UTILS_H__

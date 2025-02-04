#ifndef __DYNALLOC_H__
#define __DYNALLOC_H__

#include <stdlib.h>

// forward declaration para no hacer el include cache.h
typedef struct Cache* Cache;

/**
 *  @brief Asigna un bloque de memoria. De no contarse con memoria disponible, elimina los nodos menos recientemente utilizados de la cache hasta contar con espacio suficiente.
 * 
 * @param sz El tamaño del bloque a asignar.
 * @param cache La cache asociada a quien invoca a esta funcion. 
 */
void* dynalloc(size_t sz, Cache cache);

// todo: especificar esto. obs: ptr deberia ser nullable pero no se como settearlo xd.
void* dynrealloc(void* ptr, size_t sz, Cache cache);


#endif // __DYNALLOC_H__
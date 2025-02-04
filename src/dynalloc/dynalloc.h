#ifndef __DYNALLOC_H__
#define __DYNALLOC_H__

#include <stdlib.h>
#include <pthread.h>

// forward declaration para no hacer el include cache.h
typedef struct Cache* Cache;

/**
 *  @brief Asigna un bloque de memoria. De no contarse con memoria disponible, elimina los nodos menos recientemente utilizados de la cache hasta contar con espacio suficiente.
 * 
 * @param sz El tamaño del bloque a asignar.
 * @param lock Un puntero a un mutex que se posee al invocar la funcion. Si el puntero es no nulo, este es liberado al inicio para prevenir deadlocks, y adquirido nuevamente antes de finalizar la ejecucion.
 * @param cache La cache asociada a quien invoca a esta funcion. 
 */
void* dynalloc(size_t sz, pthread_mutex_t* lock, Cache cache);


/**
 *  @brief Cambia el tamaño del bloque de memoria al que apunta \a ptr a \a sz bytes. Los contenidos del bloque permanecen inalterados en el rango desde el inicio hasta el minimo entre los tamaños nuevo y viejo. Si el tamaño nuevo es mayor, la memoria extra no sera inicializada. Si \a ptr es NULL, el llamado es equivalente a dynalloc(sz, lock, cache). Si \a sz es 0, y \a ptr no es NULL, entonces el llamado es equivalente a free(ptr).
 * 
 * @param ptr El puntero al inicio del bloque de memoria original.
 * @param sz El nuevo tamaño del bloque a asignar.
 * @param lock Un puntero a un mutex que se posee al invocar la funcion. Si el puntero es no nulo, este es liberado al inicio para prevenir deadlocks, y adquirido nuevamente antes de finalizar la ejecucion.
 * @param cache La cache asociada a quien invoca a esta funcion. 
 */
void* dynrealloc(void* ptr, size_t sz, pthread_mutex_t* lock, Cache cache);


#endif // __DYNALLOC_H__
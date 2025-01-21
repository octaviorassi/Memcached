#ifndef __CACHE_H__
#define __CACHE_H__

#include "../helpers/results.h"

typedef struct Cache* Cache;

/**
 *  @brief Inicializa una Cache con funcion de hash \a hash y cantidad de
 *  buckets \a n_buckets.
 * 
 *  @param hash Funcion de hash a utilizar.
 *  @param n_buckets Cantidad de buckets a crear.
 * 
 *  @return Un puntero a la cache creada.
 */
Cache cache_create(HashFunction hash, int n_buckets);

/**
 *  @brief Busca el valor asociado a una clave en la cache.
 * 
 *  @param key La clave buscada.
 *  @param cache La cache objetivo
 * 
 *  @return Un LookUp result con un status indicando si la operacion fue exitosa.
 */
LookupResult cache_get(int key, Cache cache);

/**
 *  @brief Inserta un par clave-valor en la cache. Si la clave ya estaba 
 *  asociada a un valor, lo actualiza. De ser necesario, aplica la politica
 *  de desalojo preestablecida para liberar memoria.
 * 
 *  @param key La clave.
 *  @param val El valor asociado.
 *  @param cache La cache objetivo
 *  @return 0 en caso de exito, -1 si se produjo un error. 
 */
int cache_put(int key, int val, Cache cache);


/**
 *  @brief Elimina el par clave-valor asociado a \a key en la cache objetivo.
 * 
 *  @param key La clave del par.
 *  @param cache La cache objetivo.
 * 
 *  @return 0 si se elimina un par, 1 si no se encuentra el par, -1 si se
 *  produjo un error.
 */
int cache_delete(int key, Cache cache);

// todo
void cache_stats(Cache cache);


/**
 *  @brief Libera la memoria de todas las componentes de la cache, incluida
 *  esta ultima.
 * 
 *  @param cache La cache a destruir.
 */
void cache_destroy(Cache cache);

#endif // __CACHE_H__
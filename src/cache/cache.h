#ifndef __CACHE_H__
#define __CACHE_H__

#include "../dynalloc/dynalloc.h"
#include "../helpers/results.h"

typedef struct Cache* Cache;
typedef int (*HashFunction)(int);

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
 *  @param cache La cache objetivo.
 * 
 *  @return Un LookUp result con un status indicando si la operacion fue exitosa.
 */
LookupResult cache_get(void* key, Cache cache);

/**
 *  @brief Inserta un par clave-valor en la cache. Si la clave ya estaba 
 *  asociada a un valor, lo actualiza. De ser necesario, aplica la politica
 *  de desalojo preestablecida para liberar memoria.
 * 
 *  @param key La clave.
 *  @param val El valor asociado.
 *  @param cache La cache objetivo.
 *  @return 0 en caso de exito, -1 si se produjo un error. 
 */
int cache_put(void* key, size_t key_size, void* val, size_t val_size, Cache cache);


/**
 *  @brief Elimina el par clave-valor asociado a \a key en la cache objetivo.
 * 
 *  @param key La clave del par.
 *  @param cache La cache objetivo.
 * 
 *  @return 0 si se elimina un par, 1 si no se encuentra el par, -1 si se
 *  produjo un error.
 */
int cache_delete(void* key, Cache cache);

// todo
void cache_stats(Cache cache);


/**
 *  @brief Libera la memoria de todas las componentes de la cache, incluida
 *  esta ultima.
 * 
 *  @param cache La cache a destruir.
 */
void cache_destroy(Cache cache);


/**
 *  @brief Libera memoria en la cache eliminando el nodo menos utilizado. 
 * 
 *  `IMPORTANTE` El thread que invoque a esta funcion debe poseer todos los locks de zonas y ademas el lock de la LRUQueue.
 * 
 *  @param cache La cache donde se liberara memoria.
 *  @return Un aproximado del tamaño del bloque liberado.
 */
size_t cache_free_up_memory(Cache cache);


/**
 *  @brief Adquiere el lock de todos los mutex zonales de la cache. 
 * 
 *  @param cache La cache objetivo.
 *  @return 0 si es exitoso, distinto de 0 si no.
 */
int cache_lock_all_zones(Cache cache);


/**
 *  @brief Libera los locks de todos los mutex zonales de la cache. 
 * 
 *  @param cache La cache objetivo.
 *  @return 0 si es exitoso, distinto de 0 si no.
 */
int cache_unlock_all_zones(Cache cache);

/* auxiliares */

int cache_lock_zone_mutex(int bucket_number, Cache cache);
int cache_unlock_zone_mutex(int bucket_number, Cache cache);

#endif // __CACHE_H__
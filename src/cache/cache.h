#ifndef __CACHE_H__
#define __CACHE_H__

#include "../dynalloc/dynalloc.h"
#include "../helpers/results.h"

// Macro para debugging global
#define PRINT(fmt, ...) printf("[%s] " fmt "\n", __func__, ##__VA_ARGS__)

// Hash default
unsigned long kr_hash(char* key);

typedef struct Cache* Cache;
typedef unsigned long (*HashFunction)(void* );

/**
 *  @brief Inicializa una Cache con funcion de hash \a hash.
 * 
 *  @param hash Funcion de hash a utilizar.
 * 
 *  @return Un puntero a la cache creada.
 */
Cache cache_create(HashFunction hash);

/**
 *  @brief Busca el valor asociado a una clave en la cache.
 * 
 *  @param key La clave buscada.
 *  @param key_size El tamaño de la clave.
 *  @param cache La cache objetivo.
 * 
 *  @return Un LookUpResult con status indicando si la operacion fue exitosa.
 */
LookupResult cache_get(void* key, size_t key_size, Cache cache);


/**
 *  @brief Inserta un par clave-valor en la cache. Si la clave ya estaba 
 *  asociada a un valor, lo actualiza. De ser necesario, aplica la politica
 *  de desalojo preestablecida para liberar memoria.
 * 
 *  @param key La clave.
 *  @param key_size El tamaño de la clave.
 *  @param val El valor.
 *  @param val_size El tamaño del valor.
 *  @param cache La cache objetivo.
 *  @return 0 en caso de exito, -1 si se produjo un error. 
 */
int cache_put(void* key, size_t key_size, void* val, size_t val_size, Cache cache);


/**
 *  @brief Elimina el par clave-valor asociado a \a key en la cache objetivo.
 * 
 *  @param key La clave del par.
 *  @param key_size El tamaño de la clave.
 *  @param cache La cache objetivo.
 * 
 *  @return 0 si se elimina un par, 1 si no se encuentra el par, -1 si se
 *  produjo un error.
 */
int cache_delete(void* key, size_t key_size,  Cache cache);


/**
 *  @brief todo
 * 
 *  @param cache La cache objetivo.
 */
void cache_stats(Cache cache);

/**
 *  @brief Libera memoria en la cache eliminando el nodo menos utilizado. 
 * 
 *  @param cache La cache donde se liberara memoria.
 *  @return Un aproximado del tamaño del bloque liberado, o 0 si no pudo eliminarse ningun nodo.
 */
int cache_free_up_memory(Cache cache, int);


/**
 *  @brief Libera la memoria de todas las componentes de la cache, incluida
 *  esta ultima.
 * 
 *  @param cache La cache a destruir.
 */
void cache_destroy(Cache cache);



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
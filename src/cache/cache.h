#ifndef __CACHE_H__
#define __CACHE_H__

#include "cache_stats.h"
#include "../dynalloc/dynalloc.h"
#include "../helpers/results.h"


// Macro para debugging global
#define DEBUG 1
#if DEBUG == 1
    #define PRINT(fmt, ...) printf("[%s] " fmt "\n", __func__, ##__VA_ARGS__)
#else
    #define PRINT(fmt, ...) ((void)0) // Expands to nothing when DEBUG is not 1
#endif


// Hash default
unsigned long kr_hash(char* key, size_t size);

// Forward declarations para evitar los doble include.
typedef struct Cache* Cache;
typedef unsigned long (*HashFunction)(void* , size_t);

/**
 *  @brief Inicializa una Cache con funcion de hash \a hash.
 * 
 *  @param hash Funcion de hash a utilizar.
 *  @param num_threads Cantidad de threads que correran sobre la cache.
 * 
 *  @return Un puntero a la cache creada.
 */
Cache cache_create(HashFunction hash, int num_threads);


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
 * 
 *  @return 0 en caso de insertar un nuevo par clave-valor exitosamente, 1 si la clave ya estaba en la cache y solo se actualizo el valor, -1 si se produjo un error. 
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
 *  @brief Genera un reporte de estadisticas de uso de la cache objetivo.
 * 
 *  @param cache La cache objetivo.
 *  @return Un reporte con los contadores para cada operacion realizada por la cache.
 */
StatsReport cache_report(Cache cache);


/**
 *  @brief Libera memoria en la cache eliminando nodos. Comienza desde los que no fueron accedidos recientemente. Libera hasta un maximo de memoria determinado.
 * 
 *  @param cache La cache donde se liberara memoria.
 *  @param memory_goal La cantidad de memoria objetivo a liberar. 
 *  @return La cantidad de memoria liberada al eliminar, que puede ser menor que el objetivo de memoria a liberar, o -1 si se produjo un error.
 */
ssize_t cache_free_up_memory(Cache cache, size_t memory_goal);


/**
 *  @brief Obtiene el puntero a la estructura que almacena las estadisticas de la cache objetivo.
 *    
 *  @param cache Cache objetivo.
 *  @return El puntero a la estructura CacheStats.
 */
CacheStats cache_get_cstats(Cache cache);


/**
 *  @brief Libera la memoria de todas las componentes de la cache, incluida
 *  esta ultima.
 * 
 *  @param cache La cache a destruir.
 */
void cache_destroy(Cache cache);


#endif // __CACHE_H__
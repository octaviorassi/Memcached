#ifndef __HASH_H__
#define __HASH_H__


typedef struct _Cache* Cache;


// @brief:
// @param[out]:
// @param[in]:
// @return:
Cache cache_create();


// @brief: Retorna el valor asociado a la clave 'key' en la cache
int cache_get(Cache cache, int key);

// @brief: Inserta el par clave valor en la cache.
void cache_put(Cache cache, int key, int value);

// @brief: Busca la clave en la cache y, si existe, elimina la entrada y retorna 1. Retorna 0 si no existe.
int cache_delete(Cache cache, int key);

void cache_stats(Cache cache);

// @brief: Destruya la estructura cache, liberando toda memoria asociada a ella.
void cache_destroy(Cache cache);

#endif // __HASH_H__
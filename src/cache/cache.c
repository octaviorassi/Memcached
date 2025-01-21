#include <stdlib.h>
#include "cache.h"
#include "../lru/lru.h"
#include "../hashmap/hashmap.h"
#include <pthread.h>

// ? borrar
typedef pthread_mutex_t Lock;

#define N_LOCKS 10
#define N_BUCKETS 100

struct Cache {

  HashMap   map;
  
  LRUQueue  queue;
  
  int       stats;

};

// todo: pasar la funcion hash por argumento, la cantidad de buckets, etc.
Cache cache_create() { 

  // Asignar memoria para la cache
  Cache cache = malloc(sizeof(struct Cache));
  if (cache == NULL)
    return NULL;

  // Inicializar HashMap
  HashMap map = hashmap_create(placeholder_hash, N_BUCKETS);
  if (map == NULL)
    return NULL;

  // Inicializar LRUQueue
  LRUQueue queue = lru_queue_create();
  if (queue == NULL)
    return NULL;

  // todo: inicializar stats
  int stats = 0;

  cache->map   = map;
  cache->queue = queue;
  cache->stats = stats;

  return cache;
}

LookupResult cache_get(Cache cache, int key) { 

  if (cache == NULL)
    return create_error_lookup_result();

  /**
   * 1. Buscar la key en el hashmap.
   * 2. Si hittea, actualizo la prioridad en la LRUQueue y retorno un LookupResult exitoso.
   *    Si no, retorno un LookupResult con status MISS.
   * 
   * Para poder actualizar la prioridad en la LRUQueue, necesito el puntero al nodo.
   * Para ello, HashMap deberia ofrecer una funcion que devuelva el puntero en vez de la key/
   */

  // Obs: al salir de hashmap_lookup_node contamos con su mutex si el nodo fue encontrado.
  HashNode node = hashmap_lookup_node(key, cache->map);
  if (node == NULL) 
    return create_miss_lookup_result();

  int val = hashnode_get_val(node);

  // ! Problema: puede ser que entre que encuentro el resultado y actualizo su prioridad, 
  // ! otro proceso aplique la politica de desalojo y borre este nodo. No se si deberiamos evitarlo o no.
  int lru_status = lru_queue_set_most_recent(node, cache->queue);

  int lock_status = hashmap_release_key_lock(key, cache->map);

  if (lru_status != 0 || lock_status != 0)
    return create_error_lookup_result();

  return create_ok_lookup_result(val);

}

// ! La logica de este put esta mal en general, lo dejo como ejemplo nomas.
// ! Hay que ver si la clave ya estaba en la cache antes de insertar y demas.
int cache_put(Cache cache, int key, int val) { 

  if (cache == NULL)
    return -1;

  // Obtenemos el mutex asociado a la key
  if (hashmap_get_key_lock(key, cache->map) != 0)
    return -1;

  // Insertamos al HashMap
  HashNode node = hashmap_insert(key, val, cache->map);
  if (node == NULL)
    return -1;
  
  lru_queue_add_recent(node, cache->queue);

  if (hashmap_release_key_lock(key, cache->map) != 0)
    return 1;

  return 0;
}

void cache_delete(Cache cache, int key) { 

  /**
   * 0. Buscamos el puntero del nodo en el hashmap. 
   * 1. Lo limpio en la LRUQueue (no libera la memoria). 
   * 2. Elimino en HashMap.
   */

  return;
  
}

void cache_stats(Cache cache) { return; }

void cache_destroy(Cache cache) { return; }


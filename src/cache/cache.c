#include <stdlib.h>
#include "cache.h"
#include "../lru/lru.h"
#include "../hash/hash.h"
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

  HashNode node = hashmap_lookup_node(key, cache->map);
  if (node == NULL)
    return create_miss_lookup_result();

  // Si devuelve NULL hubo un error
  if (lru_queue_set_most_recent(node, cache->queue))
    return create_ok_lookup_result(lru_hash_node_get_value(node));

  return create_error_lookup_result();
}

int cache_put(Cache cache, int key, int val) { 

  if (cache == NULL)
    return -1;
    
  /**
   * 1. Inserto en HashMap
   * 2. Inserto en LRUQueue.
   * El orden importa: el insert de HashMap es el que le asigna memoria, el de LRUQueue trabaja
   * sobre un nodo ya creado.
   */


  // Insertamos al HashMap

  // hashmap_get_lock
  // hashmap_insert

  // lru_add

  // hashmap_release_lock

  HashNode node = hashmap_insert(key, val, cache->map);
  if (node == NULL)
    return -1;

  /* ******************************************** !!! PROBLEMA !!!
  
    Que pasa si:
      Proceso A llega a hacer el hashmap_insert y pierde el control
      Proceso B deletea al nodo por completo
      Proceso A retoma el control e intenta ejecutar el lru_queue_add_recent

    En ese caso, node != NULL, pero la memoria ya esta liberada. Entonces va a desreferenciar
    una zona de memoria a la que no tiene acceso. Problemas.
  
  */

  // Y le setteamos su prioridad
  if (lru_queue_add_recent(node, cache->queue) == NULL)
    return -1;

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


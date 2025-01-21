#include <stdlib.h>
#include <pthread.h>
#include "cache.h"
#include "../hashmap/hashmap.h"
#include "../lru/lru.h"

// ? borrar
typedef pthread_mutex_t Lock;

#define N_LOCKS 10
#define N_BUCKETS 100

struct Cache {

  HashMap   map;
  
  LRUQueue  queue;
  
  int       stats;

};

Cache cache_create(HashFunction hash, int n_buckets) { 

  Cache cache = malloc(sizeof(struct Cache));
  if (cache == NULL)
    return NULL;

  HashMap map = hashmap_create(hash, n_buckets);
  if (map == NULL) {
    free(cache);
    return NULL;
  }

  LRUQueue queue = lru_queue_create();
  if (queue == NULL) {
    hashmap_destroy(map);
    free(cache);
    return NULL;
  }

  // todo: pensar que va en stats, es un placeholder esto
  int stats = 0;

  cache->map   = map;
  cache->queue = queue;
  cache->stats = stats;

  return cache;

}

LookupResult cache_get(int key, Cache cache) { 

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
  LRUNode lru_status = lru_queue_set_most_recent(hashnode_get_prio(node), cache->queue);

  int lock_status = hashmap_release_key_lock(key, cache->map);

  if (lru_status != NULL || lock_status != 0)
    return create_error_lookup_result();

  return create_ok_lookup_result(val);

}

int cache_put(int key, int val, Cache cache) {
  // todo: aplicar politica de desalojo
  if (cache == NULL)
    return -1;

  // Al salir, si node != NULL entonces se posee su mutex.
  HashNode node = hashmap_update(key, val, cache->map);

  // La clave ya tenia un valor asociado en la cache. 
  // Actualizamos su prioridad y devolvemos su mutex.
  if (node != NULL) {
    lru_queue_set_most_recent(hashnode_get_prio(node), cache->queue);
    hashmap_release_key_lock(key, cache->map);
    return 0;
  }

  // Si el update no se realizo, entonces no tenemos el lock.
  // Lo adquirimos y luego insertamos.
  if (hashmap_get_key_lock(key, cache->map) != 0)
    return -1;

  node = hashmap_insert(key, val, cache->map);

  if (node == NULL) {
    hashmap_release_key_lock(key, cache->map);
    return -1;
  }

  lru_queue_set_most_recent(hashnode_get_prio(node), cache->queue);

  hashmap_release_key_lock(key, cache->map);

  return 0;

}

int cache_delete(int key, Cache cache) { 

  if (cache == NULL)
    return -1;
  
  HashNode node = hashmap_lookup_node(key, cache->map);

  // La clave no pertenecia a la cache
  if (node == NULL)
    return -1;

  // La clave pertenecia, entonces tenemos el mutex de node.
  // Lo sacamos de la cola LRU.
  lru_queue_node_clean(hashnode_get_prio(node), cache->queue);

  // Y liberamos la memoria que se le habia asignado.
  // ! Observacion: quizas Cache no deberia conocer la interfaz de hashnode, si no que
  // ! solo la de hashmap, y deberiamos tener una destroy_node en hashmap.
  hashnode_destroy(node);
  
  return 0;

}

void cache_stats(Cache cache) { return; }

void cache_destroy(Cache cache) { return; }


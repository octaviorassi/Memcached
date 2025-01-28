#include <stdlib.h>
#include <pthread.h>
#include "cache.h"
#include "../hashmap/hashnode.h"
#include "../lru/lru.h"

// ? borrar
typedef pthread_mutex_t Lock;

#define N_LOCKS 10
#define N_BUCKETS 100

struct Cache {

  HashFunction        hash_function;
  HashNode*           buckets;
  int                 n_buckets;
  pthread_mutex_t*    zone_locks[N_LOCKS];

  LRUQueue  queue;
  
  int       stats;

};

Cache cache_create(HashFunction hash, int n_buckets) { 

  Cache cache = malloc(sizeof(struct Cache));
  if (cache == NULL)
    return NULL;

  if (hashmap_init(hash, n_buckets, cache) != 0) {
    free(cache);
    return NULL;
  }

  LRUQueue queue = lru_queue_create();
  if (queue == NULL) {
    hashmap_destroy(cache);
    free(cache);
    return NULL;
  }

  // todo: pensar que va en stats, es un placeholder esto
  int stats = 0;

  cache->queue = queue;
  cache->stats = stats;

  return cache;

}

LookupResult cache_get(int key, Cache cache) { 

  if (cache == NULL)
    return create_error_lookup_result();

  // Calculamos el bucket
  int bucket_number = cache_get_bucket_number(key, cache);

  if (bucket_number < 0)
    return create_error_lookup_result();

  // Obtenemos su lock asociado y un puntero al inicio
  if (cache_lock_zone_mutex(bucket_number, cache) != 0)
    return create_error_lookup_result();

  HashNode bucket = cache->buckets[bucket_number];

  // Buscamos el nodo asociado a la key en el bucket
  HashNode node = hashnode_lookup_node(key, bucket);

  // Si no lo encontramos, devolvemos un miss.
  if (node == NULL) {
    cache_unlock_zone_mutex(bucket_number, cache);
    return create_miss_lookup_result();
  }

  // Si lo encontramos, actualizamos la prioridad, devolvemos el lock,
  // y retornamos el valor.
  int val = hashnode_get_val(node);

  // ! Problema: puede ser que entre que encuentro el resultado y actualizo su prioridad, 
  // ! otro proceso aplique la politica de desalojo y borre este nodo. No se si deberiamos evitarlo o no.
  LRUNode lru_status = lru_queue_set_most_recent(hashnode_get_prio(node), cache->queue);

  int lock_status = cache_release_key_lock(key, cache);

  if (lru_status != NULL || lock_status != 0)
    return create_error_lookup_result();

  return create_ok_lookup_result(val);

}

int cache_put(int key, int val, Cache cache) {
  // todo: aplicar politica de desalojo
  if (cache == NULL)
    return -1;

  // Al salir, si node != NULL entonces se posee su mutex.
  // HashNode node = hashmap_update(key, val, cache->map);

  int bucket_number = cache_get_bucket_number(key, cache);

  if (bucket_number < 0)
    return -1;

  if (cache_lock_zone_mutex(bucket_number, cache) != 0)
    return -1;

  HashNode bucket = cache->buckets[bucket_number];

  HashNode node = hashnode_lookup_node(key, bucket);

  // La clave ya tenia un valor asociado en la cache. 
  // Actualizamos su valor y prioridad.
  if (node != NULL) {
    hashnode_set_val(node, val);
    lru_queue_set_most_recent(hashnode_get_prio(node), cache->queue);
    cache_unlock_zone_mutex(bucket_number, cache);
    return 0;
  }

  // Si no estaba en la cache, lo insertamos.
  node = hashnode_create(key, val);

  if (node == NULL) {
    // todo: Aca debe implementarse la politica de desalojo
    cache_unlock_zone_mutex(bucket_number, cache);
    return -1;
  }

  /* Insercion:
      I.   El prev del bucket pasa a ser node
      II.  El next del node   pasa a ser bucket
      III. El bucket pasa a ser node
  */

  hashnode_set_prev(bucket, node);
  hashnode_set_next(node, bucket);
  cache->buckets[bucket_number] = node;

  lru_queue_set_most_recent(hashnode_get_prio(node), cache->queue);

  cache_unlock_zone_mutex(bucket_number, cache);

  return 0;

}

int cache_delete(int key, Cache cache) { 

  if (cache == NULL)
    return -1;
  
  /**
   *   Calculamos su bucket.
   *   Obtenemos el mutex del bucket.
   *   Buscamos el nodo en el bucket.
   *   Si no lo encontramos, liberamos el lock y retornamos.
   *   Si lo encontramos, liberamos su LRUNode asociado, destruimos
   *  el nodo, devolvemos el mutex y retornamos.
   */
  int bucket_number = cache_get_bucket_number(key, cache);

  if (bucket_number < 0)
    return -1;

  if (cache_lock_zone_mutex(bucket_number, cache) != 0)
    return -1;

  HashNode bucket = cache->buckets[bucket_number];

  HashNode node = hashnode_lookup_node(key, bucket);

  // La clave no pertenecia a la cache
  if (node == NULL) {
    cache_unlock_zone_mutex(bucket_number, cache);
    return -1; // ! deberia ser otro num asi podemos diferenciar un error de un miss
  }

  // La clave estaba en la cache: borramos de la cola LRU.
  lru_queue_node_clean(hashnode_get_prio(node), cache->queue);
  lrunode_destroy(hashnode_get_prio(node));

  // Y liberamos la memoria que se le habia asignado.
  hashnode_clean(node); // ! podria traer problemas si es el unico nodo del bucket?
  hashnode_destroy(node);
  
  cache_unlock_zone_mutex(bucket_number, cache);

  return 0;

}

void cache_stats(Cache cache) { return; }

void cache_destroy(Cache cache) { return; }


/* ************************* 
   *       AUXILIARES      * 
   ************************* 
      (no las exportamos)    */

int hashmap_init(HashFunction hash, int n_buckets, Cache cache) {

    cache->hash_function = hash;

    // Asignamos memoria para los buckets, no hace falta inicializarlos, el insert lo hara.
    HashNode* buckets = malloc(sizeof(HashNode) * n_buckets);
    if (buckets == NULL)
        return -1;

    cache->n_buckets = n_buckets;

    // Inicializamos los locks
    int mutex_error = 0;
    for (int i = 0; i < N_LOCKS; i++) 
        mutex_error = mutex_error || pthread_mutex_init(cache->zone_locks[i],NULL);

    if (mutex_error)
        return -1;

    return 0;

}

int hashmap_destroy(Cache cache) {

  if (cache == NULL)
    return 0;

  free(cache->buckets);

  for (int i = 0; i < N_LOCKS; i++)
    pthread_mutex_destroy(cache->zone_locks[i]);

  return 0;

}

int cache_get_bucket_number(int key, Cache cache) {
    if (cache == NULL)
        return -1;
    return cache->hash_function(key) % cache->n_buckets;
}


int cache_get_key_lock(int key, Cache cache) {

    if (cache == NULL)
        return -1;
    
    int bucket_number = cache_get_bucket_number(key, cache);

    return cache_lock_zone_mutex(bucket_number, cache);
    
}

int cache_release_key_lock(int key, Cache cache) {

    if (cache == NULL)
        return -1;
    
    int bucket_number = cache_get_bucket_number(key, cache);

    return cache_unlock_zone_mutex(bucket_number, cache);

}

// todo, creo que a estas no la vamos a querer exportar.
int cache_lock_zone_mutex(int bucket_number, Cache cache) {
    return 0;
}

int cache_unlock_zone_mutex(int bucket_number, Cache cache) {
    return 0;
}
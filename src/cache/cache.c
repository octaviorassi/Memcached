#include <stdlib.h>
#include <pthread.h>
#include <string.h>
#include "cache.h"
#include "../hashmap/hashnode.h"
#include "../lru/lru.h"
#include "../lru/lrunode.h"
#include "cache_stats.h"

#define N_LOCKS 10
#define N_BUCKETS 10 * N_LOCKS

struct Cache {

  // Hash
  HashFunction        hash_function;
  
  HashNode            buckets[N_BUCKETS];
  int                 n_buckets;
  pthread_mutex_t*    zone_locks[N_LOCKS];

  // LRUQueue
  LRUQueue  queue;

  // CacheStats
  CacheStats stats;

};

// Funcion de hash de K&R.
unsigned long kr_hash(char* key) {
	
  unsigned long hashval;
  unsigned long i;

  for (i = 0, hashval = 0 ; i < strlen(key) ; ++i, key++)
    hashval = *key + 31 * hashval;
  
  return hashval;
}


// Prototipos de las static para no declararlas en el .h
static int hashmap_init(HashFunction hash, Cache cache);
static int hashmap_destroy(Cache cache);

pthread_mutex_t* cache_get_key_mutex(void* key, Cache cache);
static unsigned int cache_get_bucket_number(void* key, Cache cache);
static pthread_mutex_t* cache_get_zone_mutex(unsigned int bucket_number, Cache cache);


Cache cache_create(HashFunction hash) { 

  Cache cache = malloc(sizeof(struct Cache));
  if (cache == NULL)
    return NULL;

  if (hashmap_init(hash, cache) != 0) {
    free(cache);
    return NULL;
  }

  LRUQueue queue = lru_queue_create(cache);
  if (queue == NULL) {
    hashmap_destroy(cache);
    free(cache);
    return NULL;
  }

  // todo: pensar que va en stats, es un placeholder esto
  CacheStats stats = cache_stats_create();
  if (stats == NULL) {
    hashmap_destroy(cache);
    lru_queue_destroy(queue);
    free(cache);
    return NULL;
  }

  cache->queue = queue;
  cache->stats = stats;

  return cache;

}


LookupResult cache_get(void* key, size_t key_size, Cache cache) { 

  if (cache == NULL)
    return create_error_lookup_result();

  // Calculamos el bucket
  int bucket_number = cache_get_bucket_number(key, cache);

  if (bucket_number < 0)
    return create_error_lookup_result();

  // Obtenemos su lock asociado y un puntero al inicio
  pthread_mutex_t* lock = cache_get_zone_mutex(bucket_number, cache);
  if (lock == NULL)
    return create_error_lookup_result();

  pthread_mutex_lock(lock);

  HashNode bucket = cache->buckets[bucket_number];

  // Buscamos el nodo asociado a la key en el bucket
  HashNode node = hashnode_lookup_node(key, key_size, bucket);

  // Si no lo encontramos, devolvemos un miss.
  if (node == NULL) {
    pthread_mutex_unlock(lock);
    return create_miss_lookup_result();
  }

  // Si lo encontramos, actualizamos la prioridad, devolvemos el lock,
  // y retornamos el valor.
  void* val = hashnode_get_val(node);

  lru_queue_set_most_recent(hashnode_get_prio(node), cache->queue);

  pthread_mutex_unlock(lock);

  cache_stats_get_counter_inc(cache->stats);

  return create_ok_lookup_result(val);

}


int cache_put(void* key, size_t key_size, void* val, size_t val_size, Cache cache) {

  if (cache == NULL)
    return -1;

  // Calculamos el bucket number y obtenemos su lock asociado.
  int bucket_number = cache_get_bucket_number(key, cache);
  // PRINT("bucket_number calculado para %s: %i", key, bucket_number);

  if (bucket_number < 0)
    return -1;

  pthread_mutex_t* lock = cache_get_zone_mutex(bucket_number, cache);
  if (lock == NULL)
    return -1;
  
  pthread_mutex_lock(lock);

  HashNode bucket = cache->buckets[bucket_number];
  // PRINT("Direccion del bucket donde insertaremos a %s: %p", key, bucket);

  HashNode node = hashnode_lookup_node(key, key_size, bucket);
  // PRINT("%s logro terminar la busqueda", key);

  cache_stats_put_counter_inc(cache->stats);

  // La clave ya tenia un valor asociado en la cache. 
  // Actualizamos su valor y prioridad.
  if (node != NULL) {
    hashnode_set_val(node, val, val_size, cache);
    lru_queue_set_most_recent(hashnode_get_prio(node), cache->queue);
    pthread_mutex_unlock(lock);
    return 0;
  }

  // Si no estaba en la cache, lo insertamos.
  node = hashnode_create(key, key_size, val, val_size, cache);
  // PRINT("Direccion del nodo creado para %s: %p", key, node);

  if (node == NULL) {
    pthread_mutex_unlock(lock);
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

  pthread_mutex_unlock(lock);

  cache_stats_key_counter_inc(cache->stats);

  return 0;

}


int cache_delete(void* key, size_t key_size,  Cache cache) { 

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

  unsigned int bucket_number = cache_get_bucket_number(key, cache);

  pthread_mutex_t* lock = cache_get_zone_mutex(bucket_number, cache);
  if (lock == NULL)
    return -1;

  pthread_mutex_lock(lock);

  HashNode bucket = cache->buckets[bucket_number];

  HashNode node = hashnode_lookup_node(key, key_size, bucket);

  // La clave no pertenecia a la cache
  if (node == NULL) {
    pthread_mutex_unlock(lock);
    return 1; // > 0 es un miss, < 0 es un error
  }

  // La clave estaba en la cache: borramos de la cola LRU.
  lru_queue_node_clean(hashnode_get_prio(node), cache->queue);
  lrunode_destroy(hashnode_get_prio(node));

  // PRINT("Logre eliminar de la LRU a %s.", key); 

  // Y liberamos la memoria que se le habia asignado.
  hashnode_clean(node); 

  // Si era el unico nodo del bucket, debemos settear el bucket a NULL.
  if (bucket == node)
    cache->buckets[bucket_number] = NULL;

  hashnode_destroy(node);
  
  pthread_mutex_unlock(lock);

  cache_stats_del_counter_inc(cache->stats);
  cache_stats_key_counter_dec(cache->stats);

  return 0;

}

// todo: aca llenaria algun buffer con cache_stats_show() o algo asi
void cache_stats(Cache cache) { cache_stats_show(cache->stats, NULL); }


void cache_destroy(Cache cache) { return; }


size_t cache_free_up_memory(Cache cache) {

  if (cache == NULL)
    return 0;

  lru_queue_lock(cache->queue);
  
  LRUNode lru_last_node = lru_queue_get_least_recent(cache->queue);
  HashNode hashnode = lrunode_get_hash_node(lru_last_node);
  pthread_mutex_t* zone_lock = cache_get_key_mutex(hashnode_get_key(hashnode), cache);

  // todo: pasar tambien por argumento si tengo algun lock para compararlo y no hacer el trylock en ese caso, pues ya lo tenemos.
  int got_key = pthread_mutex_trylock(zone_lock) == 0;

  while (!got_key && lru_last_node != NULL) {

    lru_last_node = lrunode_get_next(lru_last_node);

    hashnode = lrunode_get_hash_node(lru_last_node);

    zone_lock = cache_get_key_mutex(hashnode_get_key(hashnode), cache);

    got_key = pthread_mutex_trylock(zone_lock) == 0;

  }

  // Al salir del while, tengo el lock del HashNode asociado a lru_last_node o no hay mas nodos en la LRUQueue.
  // todo: agregar que vuelva a intentar? ej. que largue el lock y espere
  if (!got_key) return 0;

  // Calculamos la memoria del nodo aproximadamente.
  // todo: mejorarlo.
  size_t freed_size = hashnode_get_key_size(hashnode) +
                      hashnode_get_val_size(hashnode) +
                      sizeof(HashNode);

  // Eliminamos al LRUNode de la LRUQueue
  lru_queue_node_clean(lru_last_node, cache->queue);
  lru_queue_delete(lru_last_node, cache->queue);

  // Y lo eliminamos del hashmap.
  hashnode_clean(hashnode);
  hashnode_destroy(hashnode);

  // Soltamos ambos locks y retornamos
  pthread_mutex_unlock(zone_lock);
  lru_queue_unlock(cache->queue);

  return freed_size;

}


LRUQueue cache_get_lruqueue(Cache cache) {
  return cache == NULL ? NULL : cache->queue;
}


int cache_lock_all_zones(Cache cache) {

  if (cache == NULL)
    return -1;

  int status = 0;
  for (int i = 0; i < N_LOCKS; i++)
    status = pthread_mutex_lock(cache->zone_locks[i]) || status;

  return status;

}


int cache_unlock_all_zones(Cache cache) {

  if (cache == NULL) 
    return -1;

  int status = 0;
  for (int i = 0; i < N_LOCKS; i++)
    status = pthread_mutex_lock(cache->zone_locks[i]) || status;

  return status;

}


pthread_mutex_t* cache_get_key_mutex(void* key, Cache cache) { 

  if (cache == NULL)
    return NULL;

  unsigned int bucket_number = cache->hash_function(key) % cache->n_buckets;

  return cache->zone_locks[bucket_number];

}











/* ************************* 
   *       AUXILIARES      * 
   ************************* 
      (no las exportamos)    
*/

static int hashmap_init(HashFunction hash, Cache cache) {

    cache->hash_function = hash;

    // Asignamos memoria para los buckets, no hace falta inicializarlos, el insert lo hara.
    HashNode* buckets = dynalloc(sizeof(HashNode) * N_BUCKETS, cache);
    if (buckets == NULL)
        return -1;

    cache->n_buckets = N_BUCKETS;

    // Inicializamos los locks
    int mutex_error = 0;
    for (int i = 0; i < N_LOCKS; i++) {
      cache->zone_locks[i] = malloc(sizeof(pthread_mutex_t));
      mutex_error = mutex_error || pthread_mutex_init(cache->zone_locks[i],NULL);
    }

    if (mutex_error)
        return -1;

    return 0;

}

static int hashmap_destroy(Cache cache) {

  if (cache == NULL)
    return 0;

  // ya no es necesario el free si no los asigno dinamicamente.
  // free(cache->buckets);

  for (int i = 0; i < N_LOCKS; i++)
    pthread_mutex_destroy(cache->zone_locks[i]);

  return 0;

}

static unsigned int cache_get_bucket_number(void* key, Cache cache) {
  if (cache == NULL)
      return -1;

  return cache->hash_function(key) % cache->n_buckets;

}

static pthread_mutex_t* cache_get_zone_mutex(unsigned int bucket_number, Cache cache) {
  if (cache == NULL)
    return NULL;

  return cache->zone_locks[bucket_number % N_LOCKS];

}

#include <stdlib.h>
#include <pthread.h>
#include <string.h>
#include "cache.h"
#include "../hashmap/hash.h"
#include "../hashmap/hashnode.h"
#include "../lru/lru.h"
#include "../lru/lrunode.h"
#include <sys/types.h>

#define BUCKETS_FACTOR 100
#define ZONES_FACTOR 10

struct Cache {

  // Hash
  HashFunction        hash_function;
  HashNode*           buckets;
  int                 num_buckets;
  pthread_rwlock_t**  zone_locks;
  int                 num_zones;

  // LRUQueue
  LRUQueue  queue;

  // CacheStats
  CacheStats stats;

};


/* Prototipos de las utilidades */

static int hashmap_init(HashFunction hash, Cache cache, int num_threads);
static int hashmap_destroy(Cache cache);
static unsigned int cache_get_bucket_number(void* key, size_t size, Cache cache);
static pthread_rwlock_t* cache_get_zone_mutex(unsigned int bucket_number, Cache cache);


/* Interfaz de la Cache */

Cache cache_create(HashFunction hash, int num_threads) { 

  Cache cache = malloc(sizeof(struct Cache));
  if (cache == NULL)
    return NULL;

  if (hashmap_init(hash, cache, num_threads) != 0) {
    free(cache);
    return NULL;
  }

  LRUQueue queue = lru_queue_create();
  if (queue == NULL) {
    hashmap_destroy(cache);
    free(cache);
    return NULL;
  }

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
  unsigned int bucket_number = cache_get_bucket_number(key, key_size, cache);

  // Obtenemos el lock asociado junto con su bucket
  pthread_rwlock_t* lock = cache_get_zone_mutex(bucket_number, cache);
  if (lock == NULL)
    return create_error_lookup_result();
  
  pthread_rwlock_rdlock(lock);

  HashNode bucket = cache->buckets[bucket_number];

  // Buscamos el nodo asociado a la key en el bucket
  HashNode node = hashnode_lookup_node(key, key_size, bucket);

  // Si no lo encontramos, devolvemos un miss.
  if (node == NULL) {
    pthread_rwlock_unlock(lock);
    return create_miss_lookup_result();
  }

  // Si lo encontramos, actualizamos la prioridad, devolvemos el lock,
  // y retornamos el valor.
  void*  val  = hashnode_get_val(node);
  size_t size = hashnode_get_val_size(node);

  lru_queue_set_most_recent(hashnode_get_prio(node), cache->queue);

  pthread_rwlock_unlock(lock);

  cache_stats_get_counter_inc(cache->stats);

  return create_ok_lookup_result(val, size);

}


int cache_put(void* key, size_t key_size, void* val, size_t val_size, Cache cache) {

  if (cache == NULL)
    return -1;

  // Calculamos el bucket number
  unsigned int bucket_number = cache_get_bucket_number(key, key_size, cache);
  
  // Lockeamos su zona
  pthread_rwlock_t* lock = cache_get_zone_mutex(bucket_number, cache);
  if (lock == NULL)
    return -1;
  
  pthread_rwlock_wrlock(lock);
  
  // Y accedemos al bucket
  HashNode bucket = cache->buckets[bucket_number];

  // Buscamos el nodo asociado a la clave en el bucket correspondiente.
  HashNode node = hashnode_lookup_node(key, key_size, bucket);

  // Lo encuentre o no, la operacion de PUT se llevo a cabo.
  cache_stats_put_counter_inc(cache->stats);

  // Si la clave ya pertenecia a la cache, solo actualizamos valor y prioridad.
  if (node != NULL) {

    // Setteamos el nuevo valor, liberando la memoria liberada en el proceso
    hashnode_set_val(node, val, val_size);

    // Restamos la memoria del valor anteior
    cache_stats_allocated_memory_free(cache->stats,
                                      hashnode_get_val_size(node));

    // Sumamos la memoria de la nueva clave
    cache_stats_allocated_memory_add(cache->stats, val_size);

    // Y actualizamos la prioridad del nodo
    lru_queue_set_most_recent(hashnode_get_prio(node), cache->queue);
    
    pthread_rwlock_unlock(lock);

    return 1;

  }

  // Si la clave no estaba en la cache, la insertamos.
  node = hashnode_create(key, key_size, val, val_size, cache);
  if (node == NULL) {
    pthread_rwlock_unlock(lock);
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

  // Setteamos los valores de su LRUNode asociado y lo insertamos a la LRUQueue
  lrunode_set_bucket_number(hashnode_get_prio(node), bucket_number);
  lrunode_set_hash_node(hashnode_get_prio(node), node);

  lru_queue_set_most_recent(hashnode_get_prio(node), cache->queue);

  pthread_rwlock_unlock(lock);

  // Sumamos la memoria del nuevo nodo y contamos la nueva key
  cache_stats_allocated_memory_add(cache->stats, key_size);
  cache_stats_allocated_memory_add(cache->stats, val_size);

  cache_stats_key_counter_inc(cache->stats);
  
  return 0;

}


int cache_delete(void* key, size_t key_size,  Cache cache) { 

  if (cache == NULL)
    return -1;
  
  // Tomamos el lock asociado al nodo a eliminar
  unsigned int bucket_number = cache_get_bucket_number(key, key_size, cache);

  // Obtenemos su zona
  pthread_rwlock_t* lock = cache_get_zone_mutex(bucket_number, cache);
  if (lock == NULL)
    return -1;

  pthread_rwlock_wrlock(lock);

  // Ahora con el lock, accedemos a su bucket y lo buscamos dentro de el.
  HashNode bucket = cache->buckets[bucket_number];
  HashNode node = hashnode_lookup_node(key, key_size, bucket);

  // Independientemente del resultado, lo contamos como una operacion DEL
  cache_stats_del_counter_inc(cache->stats);

  // Si la clave no pertenecia a la cache, devolvemos el lock y retornamos.
  if (node == NULL) {
    pthread_rwlock_unlock(lock);
    return 1; 
  }

  // Si la clave estaba en la cache borramos de la cola LRU
  lru_queue_delete_node(hashnode_get_prio(node), cache->queue);

  // Lo desconectamos del hashmap
  hashnode_clean(node); 

  // Si era el unico nodo del bucket, node->next pasa a ser el bucket
  if (bucket == node)
    cache->buckets[bucket_number] = hashnode_get_next(node);

  // Y liberamos su memoria
  size_t allocated_memory = hashnode_get_key_size(node) +
                            hashnode_get_val_size(node);
  hashnode_destroy(node);
  
  pthread_rwlock_unlock(lock);

  // Decrementamos la cantidad de keys y la cantidad de memoria asignada
  cache_stats_key_counter_dec(cache->stats);
  cache_stats_allocated_memory_free(cache->stats, allocated_memory);

  return 0;

}


StatsReport cache_report(Cache cache) {
  return cache_stats_report(cache->stats);
}

ssize_t cache_free_up_memory(Cache cache, size_t memory_goal) {

  if (cache == NULL)
    return -1;

  if (lru_queue_lock(cache->queue) != 0)
    return -1;
  
  LRUNode lru_last_node = lru_queue_get_least_recent(cache->queue);

  // Si la LRU estaba vacia, devolvemos el lock y retornamos un error, pues no deberia estar liberandose memoria si no hay elementos por eliminar.
  if (lru_last_node == NULL) {
    lru_queue_unlock(cache->queue);
    return -1;
  }

  size_t freed_memory = 0;
  size_t total_freed_memory = 0;
  while (lru_last_node != NULL && total_freed_memory < memory_goal) {

    // Obtenemos el numero de bucket del ultimo nodo
    unsigned int buck_num = lrunode_get_bucket_number(lru_last_node);
    
    // Y obtenemos su zona    
    pthread_rwlock_t* zone_lock = cache_get_zone_mutex(buck_num, cache);

    // Guardamos las referencias a su siguiente y a su hashnode asociado pues, si obtenemos el lock de este nodo, lo destruiremos y las perderemos.
    HashNode hashnode = lrunode_get_hash_node(lru_last_node);
    LRUNode next_node = lrunode_get_next(lru_last_node);

    // Si el zone_lock es NULL, se produjo algun error al pedirlo.
    if (zone_lock == NULL) {
      lru_last_node = next_node;
      continue;
    }

    // Intentamos lockear la zona    
    if (pthread_rwlock_trywrlock(zone_lock) == 0) { 

      // Si logramos tomar el lock, eliminamos al LRUNode de la LRUQueue
      lru_queue_node_clean(lru_last_node, cache->queue);
      lrunode_destroy(lru_last_node);

      // Y lo eliminamos del hashmap.
      HashNode bucket = cache->buckets[buck_num];
      if (bucket == hashnode)
        cache->buckets[buck_num] = hashnode_get_next(hashnode);
      
      hashnode_clean(hashnode);

      freed_memory = hashnode_get_key_size(hashnode) +
                     hashnode_get_val_size(hashnode);
      
      total_freed_memory += freed_memory;

      hashnode_destroy(hashnode);
    
      // Soltamos el lock
      pthread_rwlock_unlock(zone_lock);
      
      // Y actualizamos las estadisticas
      cache_stats_allocated_memory_free(cache->stats, freed_memory);
      cache_stats_evict_counter_inc(cache->stats);
      cache_stats_key_counter_dec(cache->stats);

    }

    lru_last_node = next_node;

  }

  lru_queue_unlock(cache->queue);

  return freed_memory;

}


CacheStats cache_get_cstats(Cache cache) {
  if (cache != NULL) 
    return cache->stats;
  return NULL;
}


void cache_destroy(Cache cache) { 

  if (cache == NULL)
    return;
  
  // Destruimos la LRUQueue
  lru_queue_destroy(cache->queue);

  // Destruimos los nodos que queden en la hash
  hashmap_destroy(cache);

  // Y destruimos las CacheStats
  cache_stats_destroy(cache->stats);

  // Por ultimo, liberamos la cache.
  free(cache);

}


/* Funciones de utilidad no exportadas */

/**
 *  @brief Inicializa los campos asociados al HashMap de la cache, setteando a `hash` como funcion de hash.
 * 
 *  @param hash Funcion de hash a utilizar en la cache.
 *  @param cache Puntero a la cache donde se inicializara el HashMap.
 *  @return 0 si es exitoso, -1 en caso de producirse un error al inicializar el HashMap.
 */
static int hashmap_init(HashFunction hash, Cache cache, int num_threads) {

  cache->hash_function = hash;
  int num_zones = num_threads * ZONES_FACTOR;
  int num_buckets = num_zones * BUCKETS_FACTOR;

  // Creamos el doble de zonas que de threads.
  cache->zone_locks = malloc(sizeof(pthread_rwlock_t*) * num_zones);
  if (cache->zone_locks == NULL)
    return -1;

  // Y la cantidad de buckets viene dada por la cantidad de zonas por un factor 
  cache->buckets = malloc(sizeof(HashNode) * num_buckets);
  if (cache->buckets == NULL) {
    free(cache->zone_locks);
    return -1;
  }

  memset(cache->buckets, 0, sizeof(HashNode) * num_buckets);


  // Inicializamos los locks 
  int mutex_error = 0;
  for (int i = 0; i < num_zones; i++) {
    cache->zone_locks[i] = malloc(sizeof(pthread_rwlock_t));
    if (cache->zone_locks[i] == NULL) {
      free(cache->zone_locks);
      free(cache->buckets);
      for (int j = 0; j < i; j++) free(cache->zone_locks[j]);
      return -1;
    }

    mutex_error = mutex_error || pthread_rwlock_init(cache->zone_locks[i],NULL);
  }

  cache->num_zones = num_zones;
  cache->num_buckets = num_buckets;

  if (mutex_error)
      return -1;

  return 0;

}


/**
 *  @brief Destruye los campos asociados al HashMap de la cache objetivo.
 * 
 *  @param cache Puntero a la cache donde se destruira el HashMap.
 *  @return 0 si es exitoso, distinto de 0 si se produce un error al liberar memoria.
 */
static int hashmap_destroy(Cache cache) {

  if (cache == NULL)
    return 1;

  // Tomamos los locks de todas las zonas 
  int status = 0;
  for (int i = 0; i < cache->num_zones; i++)
    status = pthread_rwlock_wrlock(cache->zone_locks[i]) || status;

  // Y pasamos a recorrer cada bucket destruyendo todos los nodos.
  HashNode tmp, next;

  for (int i = 0; i < cache->num_buckets; i++) {

    tmp = cache->buckets[i];

    while (tmp) {

      next = hashnode_get_next(tmp);

      // No importa 'limpiarlos' pues vamos a destruir a todos.
      hashnode_destroy(tmp);

      tmp = next;

    }

  }

  // Destruimos todos los pthread_rwlock_t
  for (int i = 0; i < cache->num_zones; i++) {
    pthread_rwlock_destroy(cache->zone_locks[i]);
    free(cache->zone_locks[i]);
  }

  // Tambien destruimos los arrays de locks y arrays de buckets
  free(cache->zone_locks);
  free(cache->buckets);

  return 0;

}


/**
 *  @brief Calcula el numero de bucket asociado a `key` en la cache objetivo.
 * 
 *  @param key La clave de la cual queremos saber su numero de bucket.
 *  @param size La longitud de la clave.
 *  @param cache La cache objetivo.
 *  @return El numero de bucket asociado a la key en la cache.
 */
static unsigned int cache_get_bucket_number(void* key, size_t size, Cache cache) {
  return cache->hash_function(key, size) % cache->num_buckets;
}


/**
 *  @brief Obtiene un puntero al mutex asociado al numero de bucket ingresado en la cache objetivo.
 * 
 *  @param bucket_number El numero de bucket para el cual queremos obtener su mutex.
 *  @param cache La cache objetivo.
 *  @return Un puntero al mutex asociado al bucket, que es `NULL` en caso de producirse un error o no existir.
 */
static pthread_rwlock_t* cache_get_zone_mutex(unsigned int bucket_number, Cache cache) {
  if (cache == NULL)
    return NULL;

  return cache->zone_locks[bucket_number % cache->num_zones];

}


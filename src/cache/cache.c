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
unsigned long kr_hash(char* key, size_t size) {
	
  unsigned long hashval;
  unsigned long i;

  for (i = 0, hashval = 0 ; i < size ; ++i, key++)
    hashval = *key + 31 * hashval;
  
  return hashval;
}


static int hashmap_init(HashFunction hash, Cache cache);
static int hashmap_destroy(Cache cache);
static unsigned int cache_get_bucket_number(void* key, size_t size, Cache cache);
static pthread_mutex_t* cache_get_zone_mutex(unsigned int bucket_number, Cache cache);


Cache cache_create(HashFunction hash) { 

  Cache cache = malloc(sizeof(struct Cache));
  if (cache == NULL)
    return NULL;

  if (hashmap_init(hash, cache) != 0) {
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
  // ! Observacion. Aca no hay race conditions porque key no pertenece a la cache, es el puntero al buffer donde acabamos de leer la clave.
  int bucket_number = cache_get_bucket_number(key, key_size, cache);

  if (bucket_number < 0)
    return create_error_lookup_result();

  // Obtenemos el lock asociado junto con su bucket
  pthread_mutex_t* lock = cache_get_zone_mutex(bucket_number, cache);
  if (lock == NULL)
    return create_error_lookup_result();

  pthread_mutex_lock(lock);

  HashNode bucket = cache->buckets[bucket_number];

  PRINT("%ld", key_size);
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
  size_t size = hashnode_get_val_size(node);

  lru_queue_set_most_recent(hashnode_get_prio(node), cache->queue);
  // PRINT("Inserte en la LRUQueue. Contador de items de la cola: %i", lru_queue_get_count(cache->queue));

  pthread_mutex_unlock(lock);

  cache_stats_get_counter_inc(cache->stats);

  printf("size lookup %ld \n", size);
  return create_ok_lookup_result(val, size);

}


int cache_put(void* key, size_t key_size, void* val, size_t val_size, Cache cache) {

  if (cache == NULL)
    return -1;

  // Calculamos el bucket number, lockeamos su zona y obtenemos el bucket.
  // ! IMPORTANTE. Actualmente aca hay race conditions, porque para hashear necesito acceder al valor que apunta la key pero no es posible que tenga el lock al momento de hashear (pues aun no se que lock le corresponde). Esto no es un problema cuando consideramos que en la memcached real la memoria a la que apunta esta key no es posible que ya este en la cache pues la asignamos por fuera antes de insertarla.
  int bucket_number = cache_get_bucket_number(key, key_size, cache);
  if (bucket_number < 0)
    return -1;

  pthread_mutex_t* lock = cache_get_zone_mutex(bucket_number, cache);
  if (lock == NULL)
    return -1;
  

  pthread_mutex_lock(lock);

  HashNode bucket = cache->buckets[bucket_number];

  // Buscamos el nodo asociado a la clave en el bucket correspondiente.
  HashNode node = hashnode_lookup_node(key, key_size, bucket);

  // Lo encuentre o no, la operacion de PUT se llevo a cabo.
  cache_stats_put_counter_inc(cache->stats);

  // La clave ya pertenecia a la cache, actualizamos valor y prioridad.
  if (node != NULL) {
    PRINT("%ld", val_size);
    hashnode_set_val(node, val, val_size, cache);
    lru_queue_set_most_recent(hashnode_get_prio(node), cache->queue);
    pthread_mutex_unlock(lock);
    return 0;
  }
    PRINT("%ld", val_size);

  // La clave no estaba en la cache, la insertamos.
  node = hashnode_create(key, key_size, val, val_size, cache);

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

  lrunode_set_bucket_number(hashnode_get_prio(node), bucket_number);
  lrunode_set_hash_node(hashnode_get_prio(node), node);
  lru_queue_set_most_recent(hashnode_get_prio(node), cache->queue);


  pthread_mutex_unlock(lock);

  cache_stats_key_counter_inc(cache->stats);
  return 0;

}


int cache_delete(void* key, size_t key_size,  Cache cache) { 

  if (cache == NULL)
    return -1;
  
  // ! Aca tambien hay race conditions al acceder a key. Nuevamente, no es un problema cuando podemos asumir que key no es un puntero que forme parte de la cache.
  unsigned int bucket_number = cache_get_bucket_number(key, key_size, cache);

  pthread_mutex_t* lock = cache_get_zone_mutex(bucket_number, cache);
  if (lock == NULL)
    return -1;

  pthread_mutex_lock(lock);

  HashNode bucket = cache->buckets[bucket_number];

  // Buscamos si la clave ya pertenecia al bucket.
  HashNode node = hashnode_lookup_node(key, key_size, bucket);

  // La clave no pertenecia a la cache, devolvemos el lock y retornamos.
  if (node == NULL) {
    pthread_mutex_unlock(lock);
    return 1; // > 0 es un miss, < 0 es un error
  }

  // ? Este orden es correcto? 
  // ! No esta bueno que la cache tenga que lockear la LRU para limpiar el nodo, pero tampoco podemos hacer que lru_queue_node_clean sea thread-safe.

  // La clave estaba en la cache: borramos de la cola LRU.
  lru_queue_delete_node(hashnode_get_prio(node), cache->queue);

  // Lo desconectamos del hashmap y liberamos su memoria
  hashnode_clean(node); 

  // Si era el unico nodo del bucket, node->next pasa a ser el bucket
  if (bucket == node)
    cache->buckets[bucket_number] = hashnode_get_next(node);

  // ! El destroy podria ser posterior a liberar el mutex realmente.
  hashnode_destroy(node);
  
  pthread_mutex_unlock(lock);

  cache_stats_del_counter_inc(cache->stats);
  cache_stats_key_counter_dec(cache->stats);

  return 0;

}


// todo: aca llenaria algun buffer con cache_stats_show() o algo asi
void cache_stats(Cache cache) {
  if (cache) cache_stats_show(cache->stats, NULL);
}


int cache_free_up_memory(Cache cache, int number_to_free) {

  if (cache == NULL)
    return -1;

  if (lru_queue_lock(cache->queue) != 0)
    return -1;
  
  LRUNode lru_last_node = lru_queue_get_least_recent(cache->queue);

  // La LRU estaba vacia
  if (lru_last_node == NULL) {
    PRINT("La LRUQueue estaba vacia. El contador de elementos da: %i", lru_queue_get_count(cache->queue));
    lru_queue_unlock(cache->queue);
    return -1;
  }

  int cant = 0;

  while (lru_last_node != NULL && cant < number_to_free) {

    pthread_mutex_t* zone_lock = cache_get_zone_mutex(lrunode_get_bucket_number(lru_last_node), cache);

    // Si el zone_lock es NULL, se produjo algun error al pedirlo.
    if (zone_lock == NULL) continue;

    unsigned int buck_num = lrunode_get_bucket_number(lru_last_node);
    HashNode bucket = cache->buckets[buck_num];

    LRUNode next_node = lrunode_get_next(lru_last_node);
    HashNode hashnode = lrunode_get_hash_node(lru_last_node);
    
    if (pthread_mutex_trylock(zone_lock) == 0) { // Obtuviste el lock

      // Eliminamos al LRUNode de la LRUQueue
      lru_queue_node_clean(lru_last_node, cache->queue);
      lrunode_destroy(lru_last_node);

      // Y lo eliminamos del hashmap.
      if (bucket == hashnode)
        cache->buckets[buck_num] = hashnode_get_next(hashnode);
      hashnode_clean(hashnode);
      hashnode_destroy(hashnode);
    
      pthread_mutex_unlock(zone_lock);
      cache_stats_evict_counter_inc(cache->stats);

      cant++;

      PRINT("Expulse un nodo del bucket numero %i.", buck_num);

    }

    lru_last_node = next_node;
  }
  
  lru_queue_unlock(cache->queue);
  return cant;

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



/* ************************* 
   *       AUXILIARES      * 
   ************************* 
      (no las exportamos)    
*/

/**
 *  @brief Inicializa los campos asociados al HashMap de la cache, setteando a `hash` como funcion de hash.
 * 
 *  @param hash Funcion de hash a utilizar en la cache.
 *  @param cache Puntero a la cache donde se inicializara el HashMap.
 *  @return 0 si es exitoso, -1 en caso de producirse un error al inicializar el HashMap.
 */
static int hashmap_init(HashFunction hash, Cache cache) {

  // todo: emprolijar manejo de memoria, liberar cuando retorna, etc.
  
  cache->hash_function = hash;

  memset(cache->buckets, 0, sizeof(HashNode) * N_BUCKETS);

  cache->n_buckets = N_BUCKETS;

  // Inicializamos los locks 
  // ? tiene sentido asignar dinamicamente los locks, si ya se que van a ser N_LOCKS?
  int mutex_error = 0;
  for (int i = 0; i < N_LOCKS; i++) {
    cache->zone_locks[i] = malloc(sizeof(pthread_mutex_t));
    if (cache->zone_locks[i] == NULL)
      return -1;

    mutex_error = mutex_error || pthread_mutex_init(cache->zone_locks[i],NULL);
  }

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

  // Tomamos todos los locks
  int status = 0;
  for (int i = 0; i < N_LOCKS; i++)
    status = pthread_mutex_lock(cache->zone_locks[i]) || status;

  // Y pasamos a recorrer cada bucket destruyendo todos los nodos.
  HashNode tmp, next;

  for (int i = 0; i < N_BUCKETS; i++) {

    tmp = cache->buckets[i];

    while (tmp) {

      next = hashnode_get_next(tmp);

      // No importa 'limpiarlos' pues vamos a destruir a todos.
      hashnode_destroy(tmp);

      tmp = next;

    }

  }

  // Y destruimos todos los pthread_mutex_t
  for (int i = 0; i < N_LOCKS; i++) {
    pthread_mutex_destroy(cache->zone_locks[i]);
    free(cache->zone_locks[i]);
  }


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
  if (cache == NULL)
      return -1;

  return cache->hash_function(key, size) % cache->n_buckets;

}

/**
 *  @brief Obtiene un puntero al mutex asociado al numero de bucket ingresado en la cache objetivo.
 * 
 *  @param bucket_number El numero de bucket para el cual queremos obtener su mutex.
 *  @param cache La cache objetivo.
 *  @return Un puntero al mutex asociado al bucket, que es `NULL` en caso de producirse un error o no existir.
 */
static pthread_mutex_t* cache_get_zone_mutex(unsigned int bucket_number, Cache cache) {
  if (cache == NULL)
    return NULL;

  return cache->zone_locks[bucket_number % N_LOCKS];

}

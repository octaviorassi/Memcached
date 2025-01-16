#include <stdlib.h>
#include "cache.h"
#include "../lru/lru.h"
#include "../list/list.h"
#include <pthread.h>

typedef pthread_mutex_t Lock;

#define N_LOCKS 10
#define N_BUCKETS 100

struct _Cache {

  HashBucket* buckets;
  int         n_buckets;
  Lock        zone_locks[N_LOCKS];
  LRUQueue    queue;
};

// Funcion de hash placeholder
static unsigned long hash_function(char* key) {
	
  unsigned long hashval;
  unsigned long i;

  for (i = 0, hashval = 0 ; i < strlen(key) ; ++i, key++)
    hashval = *key + 31 * hashval;
  
  return hashval;
}

Cache cache_create() { return NULL; }

int cache_get(Cache cache, int key) { 

  // Hasheo

  // Hago un get al bucket correspondiente

  // Actualizo la prioridad

  // Retorno el valor
  return 0;
}

void cache_put(Cache cache, int key, int value) { 

  // Hasheo la key

  // Inserto en el bucket correspondiente

  // Inserto en al principio de la LRUQueue

  return;
}

// ? cache_node_delete: llama a lru_node_delete y hash_node_delete ???? ver como hacer esto

int cache_delete(Cache cache, int key) { 

  // Hasheo la key

  // Busco el nodo en el bucket correspondiente
  int found = 0;

  //? Si lo encontre, llamo a deletear el nodo con cache_node_delete 

  return found;
}


void cache_stats(Cache cache) { return; }

// ! Siguiendo la logica que definimos antes, debe destruir solo la LRUQueue y NO la HashBucket
// ! Es mejor destruir desde la LRUQueue a los Buckets ya que la LRU es una sola y puede recorrerse mas facilmente
void cache_destroy(Cache cache) { return; }




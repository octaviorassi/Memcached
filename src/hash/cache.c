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

Cache cache_create() { return NULL; }
int cache_get(Cache cache, int key) { return 0; }
void cache_put(Cache cache, int key, int value) { return; }
void cache_delete(Cache cache, int key) { return; }
void cache_stats(Cache cache) { return; }

// ! Siguiendo la logica que definimos antes, debe destruir solo la LRUQueue y NO la HashBucket
void cache_destroy(Cache cache) { return; }




#include <stdlib.h>
#include "cache.h"
#include "../lru/lru.h"
#include "../hash/hash.h"
#include <pthread.h>

typedef pthread_mutex_t Lock;

#define N_LOCKS 10
#define N_BUCKETS 100

struct Cache {

  HashMap   map;
  
  LRUQueue  queue;
  
  int       stats;

};

Cache cache_create() { return NULL; }

int cache_get(Cache cache, int key) { return 0; }

void cache_put(Cache cache, int key, int value) { return; }

void cache_delete(Cache cache, int key) { return; }

void cache_stats(Cache cache) { return; }

void cache_destroy(Cache cache) { return; }


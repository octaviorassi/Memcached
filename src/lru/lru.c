#include "lru.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>


struct LRUQueue {

  LRUNode start;
  LRUNode end;

  pthread_mutex_t lock;

};

inline LRUQueue lru_init() { return NULL; };

LRUNode lru_add_to_head(LRUNode node, LRUQueue q) { return NULL; }

LRUNode lru_node_clean(LRUNode node) { return NULL; }

LRUNode lru_move_to_head(LRUNode node, LRUQueue q) { return NULL; }

LRUNode lru_queue_get_last(LRUQueue q) { return NULL; }



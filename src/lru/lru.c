#include "lru.h"
#include "../list/list.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

struct _LRUNode {

  struct _LRUNode* prev;
  struct _LRUNode* next;

  HashNode hashNode;
};


struct _LRUQueue {

  pthread_mutex_t lock;

  LRUNode start;
  LRUNode end;

};

inline LRUQueue lru_init() { return NULL; };

LRUNode lru_insert(LRUQueue q, int key, int value) {

  return NULL;
}

void lru_free_space(LRUQueue q, int key);

void lru_delete(LRUQueue q, int k);

void lru_destroy(LRUQueue q);


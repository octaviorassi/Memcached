#include "list.h"
#include "../lru/lru.h"

struct _HashNode {

  int key;
  int value;

  struct _HashNode* prev;
  struct _HashNode* next;

  LRUNode priority;
  
};


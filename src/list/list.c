#include "list.h"
#include "../lru/lru.h"
#include <stdlib.h>

struct _HashNode {

  int key;
  int value;

  struct _HashNode* prev;
  struct _HashNode* next;

  LRUNode priority;
  
};

void hash_node_destroy(HashNode node) {

  if (node == NULL) {
    return;
  }

  // ! No destruyo el LRUNode, pues asumo que se llamara a destruir sobre la LRUQueue
  free(node);

}
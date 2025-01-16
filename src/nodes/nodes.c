#include "nodes.h"
#include <stdlib.h>

struct LRUHashNode {

  int key;
  int value;

  struct LRUHashNode *lru_prev, *lru_next;

  struct LRUHashNode *hash_prev, *hash_next;

};


// TODO
LRUHashNode lru_hash_node_create(int key, int val) { return NULL; }


// TODO
int lru_hash_node_destroy(LRUHashNode node) { return 0; }



/*
  ********************************
  * Getters para los LRUHashNode *
  ******************************** 
*/

int lru_hash_node_get_key(const struct LRUHashNode* node) {
  return node->key;
}

int lru_hash_node_get_value(const struct LRUHashNode* node) {
  return node->value;
}

// @brief Obtiene el nodo siguiente en la lista LRU. Retorna NULL si no existe siguiente.
LRUHashNode lru_hash_node_get_lru_next(const LRUHashNode node) {
  if (node == NULL) {
    return NULL;
  }
  return node->lru_next;
}

LRUHashNode lru_hash_node_get_lru_prev(const LRUHashNode node) {
  if (node == NULL) {
    return NULL;
  }
  return node->lru_prev;
}

LRUHashNode lru_hash_node_get_hash_next(const LRUHashNode node) {
  if (node == NULL) {
    return NULL;
  }
   return node->hash_next;
}

LRUHashNode lru_hash_node_get_hash_prev(const LRUHashNode node) {
  if (node == NULL) {
    return NULL;  
  }
  return node->hash_prev;
}





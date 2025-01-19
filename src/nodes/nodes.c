#include "nodes.h"
#include <stdlib.h>

struct LRUHashNode {

  int key;
  int val;

  struct LRUHashNode *lru_prev, *lru_next;

  struct LRUHashNode *hash_prev, *hash_next;

};


LRUHashNode lru_hash_node_create(int key, int val) { 
  
  LRUHashNode node = malloc(sizeof(struct LRUHashNode));

  if (node == NULL)
    return NULL;

  // Inicializamos todos los punteros a NULL y setteamos los valores de key y val
  memset(node, 0, sizeof(struct LRUHashNode));

  node->key = key;
  node->val = val;

  return node;
}


int lru_hash_node_destroy(LRUHashNode node) { 

  if (node == NULL) 
    return 0;

  free(node);

  return 0;

}


LookupResult lru_hash_node_lookup(int key, LRUHashNode node) {
  LRUHashNode lookup_node = lru_hash_node_lookup_node(key, node);
  return lookup_node == NULL ? create_miss_lookup_result() :
                               create_ok_lookup_result(lookup_node->val);
}

LRUHashNode lru_hash_node_lookup_node(int key, LRUHashNode node) {

  while (node != NULL) {
  
    if (key == lru_hash_node_get_key(node))
      return node;

    node = lru_hash_node_get_hash_next(node);
  
  }

  return NULL;
}

/*
  ********************************
  * Getters para los LRUHashNode *
  ******************************** 
*/

int lru_hash_node_get_key(const struct LRUHashNode* node) {
  return node->key;
}

int lru_hash_node_get_value(const struct LRUHashNode* node) {
  return node->val;
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


/*
  ********************************
  * Setters para los LRUHashNode *
  ******************************** 
*/

void lru_hash_node_set_key(LRUHashNode node, int key) {
  if (node != NULL) {
    node->key = key;
  }
}

void lru_hash_node_set_value(LRUHashNode node, int val) {
  if (node != NULL) {
    node->val = val;
  }
}

void lru_hash_node_set_lru_next(LRUHashNode node, LRUHashNode next) {
  if (node != NULL) {
    node->lru_next = next;
  }
}

void lru_hash_node_set_lru_prev(LRUHashNode node, LRUHashNode prev) {
  if (node != NULL) {
    node->lru_prev = prev;
  }
}

void lru_hash_node_set_hash_next(LRUHashNode node, LRUHashNode next) {
  if (node != NULL) {
    node->hash_next = next;
  }
}

void lru_hash_node_set_hash_prev(LRUHashNode node, LRUHashNode prev) {
  if (node != NULL) {
    node->hash_prev = prev;
  }
}




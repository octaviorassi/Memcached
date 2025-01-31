#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "hashnode.h"

struct HashNode {
    int key, val;
    size_t key_size, val_size;

    struct HashNode* prev;
    struct HashNode* next;
    struct LRUNode* prio;
};

HashNode hashnode_create(int key, int val) {

    HashNode node = malloc(sizeof(struct HashNode));
    if (node == NULL)
        return NULL;

    LRUNode prio = lrunode_create();
    if (prio == NULL) {
        free(node);
        return NULL;
    }

    memset(node, 0, sizeof(struct HashNode));

    node->key   = key;
    node->val   = val;
    node->prio  = prio;

    return node;

}

void hashnode_destroy(HashNode node) { 

    if (node == NULL)
        return;

    // lrunode_destroy(node->prio);
    
    free(node);

}

LookupResult hashnode_lookup(int key, HashNode node) { 

    if (node == NULL)
        return create_miss_lookup_result();

    int found = hashnode_get_key(node) == key;

    while (node && !found) {
        node  = hashnode_get_next(node);
        found = hashnode_get_key(node) == key;
    }

    return found ? create_ok_lookup_result(hashnode_get_val(node)) :
                   create_miss_lookup_result();
}

HashNode hashnode_lookup_node(int key, HashNode node) {

    if (node == NULL)
        return NULL;

    int found = hashnode_get_key(node) == key;

    while (node && !found) {
        node  = hashnode_get_next(node);
        found = hashnode_get_key(node) == key;
    }

    // Si no lo encontre, node es NULL. Si lo encontre, node es el buscado.
    return node;
    
}

int hashnode_clean(HashNode node) {

  if (node == NULL)
    return -1;
  
  // Desconectamos y reconectamos los adyacentes
  HashNode prev = hashnode_get_prev(node);
  HashNode next = hashnode_get_next(node);

  hashnode_set_next(prev, next);
  hashnode_set_prev(next, prev);

  return 0;

}


int hashnode_get_key(HashNode node) {
    return node->key;
}

void hashnode_set_key(HashNode node, int key) {
    if (node != NULL) {
        node->key = key;
    }
}

int hashnode_get_val(HashNode node) {
    return node->val;
}

void hashnode_set_val(HashNode node, int val) {
    if (node != NULL) {
        node->val = val;
    }
}

size_t hashnode_get_key_size(HashNode node) {
    return node->key_size;
}

void hashnode_set_key_size(HashNode node, size_t key_size) {
    if (node != NULL) {
        node->key_size = key_size;
    }
}

size_t hashnode_get_val_size(HashNode node) {
    return node->val_size;
}

void hashnode_set_val_size(HashNode node, size_t val_size) {
    if (node != NULL) {
        node->val_size = val_size;
    }
}

HashNode hashnode_get_prev(HashNode node) {
    if (node == NULL) {
        return NULL;
    }
    return node->prev;
}

void hashnode_set_prev(HashNode node, HashNode prev) {
    if (node != NULL) {
        node->prev = prev;
    }
}

HashNode hashnode_get_next(HashNode node) {
    if (node == NULL) {
        return NULL;
    }
    return node->next;
}

void hashnode_set_next(HashNode node, HashNode next) {
    if (node != NULL) {
        node->next = next;
    }
}

LRUNode hashnode_get_prio(HashNode node) {
    if (node == NULL) {
        return NULL;
    }
    return node->prio;
}

void hashnode_set_prio(HashNode node, LRUNode prio) {
    if (node != NULL) {
        node->prio = prio;
    }
}
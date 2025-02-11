#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <pthread.h>
#include "hashnode.h"
#include "../lru/lrunode.h"

struct HashNode {
    void* key;
    void* val;
    size_t key_size, val_size;

    struct HashNode* prev;
    struct HashNode* next;
    struct LRUNode* prio;
};


int hashnode_keys_equal(const void* key_a, size_t size_a, const void* key_b, size_t size_b);


HashNode hashnode_create(void* key, size_t key_size, void* val, size_t val_size, Cache cache) {

    // No es posible insertar un par sin una key que lo identifique.
    if (key == NULL) return NULL;

    HashNode node = dynalloc(sizeof(struct HashNode), cache);
    if (node == NULL)
        return NULL;

    memset(node, 0, sizeof(struct HashNode));

    // ? cambiar esto? capaz hacer una funcion en lrunode.c que se encargue
    LRUNode prio = lrunode_create(cache);
    if (prio == NULL) {
        free(node);
        return NULL;
    }

    // Setteamos la clave
    node->key = key;
    node->key_size = key_size;

    // Asignamos memoria y setteamos el valor
    node->val = val;
    node->val_size;

    // Setteamos la prioridad
    node->prio  = prio;

    return node;

}

void hashnode_destroy(HashNode node) { 

    if (node == NULL)
        return;

    // Liberamos la clave y el valor.
    if (node->key)
        free(node->key);
    
    if (node->val)
        free(node->val);

    free(node);

}

LookupResult hashnode_lookup(void* key, size_t size, HashNode node) { 

    while (node) {
        if (hashnode_keys_equal(key, size, node->key, node->key_size))
            return create_ok_lookup_result(node->val);

        node = node->next;
    }

    return create_miss_lookup_result();

}

HashNode hashnode_lookup_node(void* key, size_t size, HashNode node) {

    while (node) {
        if (hashnode_keys_equal(key, size, node->key, node->key_size))
            return node;

        node = node->next;
    }

    return NULL;    
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

// todo: ver como las vamos a comparar.
int hashnode_keys_equal(const void* key_a, size_t size_a, const void* key_b, size_t size_b) { 

    // PRINT("Comparacion: %s vs %s. Resultado: %i", key_a, key_b, size_a == size_b && memcmp(key_a, key_b, size_a) == 0);

    if (key_a == NULL || key_b == NULL)
        return 0;
    
    return size_a == size_b && (memcmp(key_a, key_b, size_a) == 0);

}






void* hashnode_get_key(HashNode node) {
    return node ? node->key : NULL;
}

int hashnode_set_key(HashNode node, void* key, size_t new_key_size, Cache cache) {

    if (node == NULL)
        return -1;

    if (node->key)
        free(node->key);

    node->key = key;
    node->key_size = new_key_size;

    return 0;

}

void* hashnode_get_val(HashNode node) {
    return node ? node->val : NULL;
}

int hashnode_set_val(HashNode node, void* val, size_t new_val_size, Cache cache) {

    if (node == NULL)
        return -1;

    if (node->val)
        free(node->val);
    
    node->val = val;
    node->val_size = new_val_size;

    return 0;

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
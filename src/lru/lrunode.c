#include <string.h>
#include <stdio.h>
#include "lrunode.h"
#include "../dynalloc/dynalloc.h"


struct LRUNode {
    struct LRUNode* prev;
    struct LRUNode* next;

    HashNode hash_node;
    unsigned int bucket_number;

};

LRUNode lrunode_create(Cache cache) {

    LRUNode node = dynalloc(sizeof(struct LRUNode), cache);
    if (node == NULL)
        return NULL;

    memset(node, 0, sizeof(struct LRUNode));

    return node;

}

int lru_node_is_clean(LRUNode node) {
    if  (node == NULL) return 1;
    return  node->prev == NULL &&
            node->next == NULL;
}

void lrunode_destroy(LRUNode node) {
    if (node != NULL)
        free(node);
}

LRUNode lrunode_get_prev(LRUNode node) {
    if (node == NULL) {
        return NULL;
    }
    return node->prev;
}

void lrunode_set_prev(LRUNode node, LRUNode prev) {
    if (node != NULL) {
        node->prev = prev;
    }
}

LRUNode lrunode_get_next(LRUNode node) {
    if (node == NULL) {
        return NULL;
    }
    return node->next;
}

void lrunode_set_next(LRUNode node, LRUNode next) {
    if (node != NULL) {
        node->next = next;
    }
}

HashNode lrunode_get_hash_node(LRUNode node) {
    if (node == NULL) {
        return NULL;
    }
    return node->hash_node;
}

void lrunode_set_hash_node(LRUNode node, HashNode hash_node) {
    if (node != NULL) {
        node->hash_node = hash_node;
    }
}

void lrunode_set_bucket_number(LRUNode node, unsigned int bucket_number) {
    if (node != NULL)
        node->bucket_number = bucket_number;
}

unsigned int lrunode_get_bucket_number(LRUNode node) {
    return node == NULL? 0 : node->bucket_number;
}


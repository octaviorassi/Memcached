#include <stdio.h>
#include <string.h>
#include "hashnode.h"

struct HashNode {
    int key, val;
    struct HashNode* prev;
    struct HashNode* next;
};

HashNode hashnode_create(int key, int val) {

    HashNode node = malloc(sizeof(struct HashNode));

    if (node == NULL)
        return NULL;

    memset(node, 0, sizeof(struct HashNode));

    node->key = key;
    node->val = val;

    return node;

}

void hashnode_destroy(HashNode node) { return; }

LookupResult hashnode_lookup(int key, HashNode node) { return create_miss_lookup_result(); }

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
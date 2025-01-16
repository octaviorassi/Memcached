#ifndef __LIST_H__
#define __LIST_H__

typedef struct _HashNode* HashNode;

typedef struct HashNode HashBucket;

// @brief: Libera un HashNode de manera no-recursiva.
void hash_node_destroy(HashNode node);

int hash_node_get_value(HashNode node);

int hash_node_get_key(HashNode node);

#endif // __LIST_H__
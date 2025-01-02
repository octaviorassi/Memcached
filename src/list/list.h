#ifndef __LIST_H__
#define __LIST_H__

typedef struct _HashNode* HashNode;

typedef struct _LRUQueue* HashBucket;

// @brief: Libera un HashNode de manera no-recursiva.
void hash_node_destroy(HashNode node);


#endif // __LIST_H__
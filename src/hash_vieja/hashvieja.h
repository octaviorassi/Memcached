#ifndef __HASH_H__
#define __HASH_H__


typedef struct _HashMap HashMap;


HashMap* hash_create();

void hash_init(HashMap* hash);

void hash_insert(HashMap* hash, char* key, char* value);

void hash_delete (HashMap* hash, char* key);

char* hash_search(HashMap* hash, char* key);

#endif // __HASH_H__
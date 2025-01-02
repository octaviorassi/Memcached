#ifndef __HASH_H__
#define __HASH_H__


typedef struct _Cache* Cache;


// @brief:
// @param[out]:
// @param[in]:
// @return:
Cache cache_create();

int cache_get(Cache cache, int key);

void cache_put(Cache cache, int key, int value);

void cache_delete(Cache cache, int key);

void cache_stats(Cache cache);

void cache_destroy(Cache cache);

#endif // __HASH_H__
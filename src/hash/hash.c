#include "hash.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#define N_LOCKS 10

struct HashMap {

    HashFunction    hash_function;

    Bucket*         buckets;
    int             n_buckets;
    pthread_mutex_t zone_locks[N_LOCKS];

};

HashNode hashmap_insert(int key, int val, HashMap map) { return NULL; }

int hashmap_lookup(int key, HashMap map) { return 0; }

int hashmap_delete(int key, HashMap map) { return 0; }

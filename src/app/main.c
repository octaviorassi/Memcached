#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <time.h>
#include <sys/resource.h>
#include <semaphore.h>

#include "cache/cache.h"

#define KEY_SIZE 12
#define KEY_COUNT 1000
#define VAL_SIZE sizeof(int)
#define VAL_COUNT 150

#define MEGABYTE 1000000
#define GIGABYTE 1000 * MEGABYTE
#define MEMORY_LIMIT 116 * MEGABYTE

// Struct to pass multiple arguments to the thread function
typedef struct {
    int thread_id;
    Cache cache;  // Cache is already a pointer (struct Cache*)
} ThreadArgs;


sem_t turnstile;

// Thread function to perform cache operations
void* thread_func(void* arg) {
    
    // Initial turnstile so it does not run out of memory before creating threads.
    sem_wait(&turnstile);
    sem_post(&turnstile);

    ThreadArgs* args = (ThreadArgs*)arg;
    int thread_id = args->thread_id;
    Cache cache = args->cache;  
    
    char* keys[KEY_COUNT];
    char* keysGet[KEY_COUNT];
    int* vals[VAL_COUNT];

    // Generate unique keys and values for this thread
    for (int i = 0; i < KEY_COUNT; i++) {
        
        keys[i] = dynalloc(KEY_SIZE, cache);
        if (!keys[i]) {
            PRINT("Thread %d: failed to allocate memory for key %d", thread_id, i);
            return NULL;
        }
        
        keysGet[i] = dynalloc(KEY_SIZE, cache);
        if (!keysGet[i]) {
            PRINT("Thread %d: failed to allocate memory for key %d", thread_id, i);
            return NULL;
        }

        vals[i] = dynalloc(VAL_SIZE, cache);
        if (!vals[i]) {
            PRINT("Thread %d: failed to allocate memory for value %d", thread_id, i);
            free(keys[i]);  // Free the key if value allocation fails
            return NULL;
        }

        // Generate a unique key for this thread
        snprintf(keys[i], KEY_SIZE, "key_%d_%d", thread_id, i);
        snprintf(keysGet[i], KEY_SIZE, "key_%d_%d", thread_id, i);
        *vals[i] = thread_id * KEY_COUNT + i;  // Unique value for each key


        // ! Hago strlen sobre keysGet[i] porque puede que para cuando haga ese strlen ya me hayan tenido que liberar el keys[i].
        int result = cache_put(keys[i], strlen(keysGet[i]), vals[i], VAL_SIZE, cache);
        PRINT("Inserte el par clave valor numero %i.", i);
        if (result != 0) {
            PRINT("Thread %d: failed to insert key-value pair: %s / %i", thread_id, keys[i], *vals[i]);
        }

        if (i == KEY_COUNT / 2)
            cache_stats(cache);
        
    }


    cache_stats(cache);



    // // Retrieve keys from the cache
    // for (int i = 0; i < KEY_COUNT; i++) {
    //     LookupResult lr = cache_get(keysGet[i], strlen(keysGet[i]), cache);

    //     /*
    //     if (lookup_result_is_ok(lr)) {
    //         int retrieved_value = *((int*)lookup_result_get_value(lr));

    //         // ! Esto puede dar segfault si ya se libero vals[i].
    //         if (retrieved_value != *vals[i]) {
    //             PRINT("Thread %d: cGET mismatch for key %s: expected %i, got %i", thread_id, keys[i], *vals[i], retrieved_value);
    //         }

    //         // PRINT("Thread %d: got the pair %s / %i", thread_id, keys[i], retrieved_value);

    //     }  else {
    //         PRINT("Thread %d: Failed to GET the key %s", thread_id, keys[i]);
    //     } */
    // }


    // Destroy all entries.
    /*
    for (int i = 0; i < KEY_COUNT; i++) {
        int result = cache_delete(keys[i], strlen(keys[i]), cache);
        if (result < 0) {
            PRINT("Thread %d: failed to delete key: %s", thread_id, keys[i]);
        }
    }
    */
   
    return NULL;
}

void set_memory_limit(size_t limit_bytes) {
    struct rlimit limit;
    limit.rlim_cur = limit_bytes;  // Soft limit
    limit.rlim_max = limit_bytes;  // Hard limit

    if (setrlimit(RLIMIT_AS, &limit) != 0) {
        perror("setrlimit failed");
        exit(EXIT_FAILURE);
    }
}


int main() {

    setbuf(stdout, NULL);

    set_memory_limit(MEMORY_LIMIT);

    sem_init(&turnstile, 0, 1);
    sem_wait(&turnstile);

    // Create the cache with a larger size to handle more entries
    Cache cache = cache_create((HashFunction)kr_hash);
    if (!cache) {
        PRINT("Failed to create cache");
        return 1;
    }

    const int NUM_THREADS = 9;
    pthread_t threads[NUM_THREADS];
    ThreadArgs thread_args[NUM_THREADS];

    // Create threads
    for (int i = 0; i < NUM_THREADS; i++) {
        thread_args[i].thread_id = i;
        thread_args[i].cache = cache;  

        int result = pthread_create(&threads[i], NULL, thread_func, &thread_args[i]);
        if (result != 0) {
            PRINT("Failed to create thread %d", i);
            return 1;
        }
    }

    sem_post(&turnstile);

    // Wait for all threads to finish
    for (int i = 0; i < NUM_THREADS; i++) {
        pthread_join(threads[i], NULL);
    }

    // Show cache statistics
    cache_stats(cache);

    // And destroy it.
    cache_destroy(cache);

    return 0;
}
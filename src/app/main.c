#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <time.h>

#include "cache/cache.h"

#define KEY_SIZE 16
#define KEY_COUNT 100  // Increased number of keys
#define VAL_SIZE sizeof(int)
#define VAL_COUNT 100 // Increased number of values

// Struct to pass multiple arguments to the thread function
typedef struct {
    int thread_id;
    Cache cache;  // Cache is already a pointer (struct Cache*)
} ThreadArgs;

// Thread function to perform cache operations
void* thread_func(void* arg) {
    ThreadArgs* args = (ThreadArgs*)arg;
    int thread_id = args->thread_id;
    Cache cache = args->cache;  // Cache is already a pointer, no need to dereference
    char* keys[KEY_COUNT];
    int* vals[VAL_COUNT];

    // Generate unique keys and values for this thread
    for (int i = 0; i < KEY_COUNT; i++) {
        keys[i] = malloc(KEY_SIZE);
        if (!keys[i]) {
            PRINT("Thread %d: failed to allocate memory for key %d", thread_id, i);
            return NULL;
        }

        vals[i] = malloc(VAL_SIZE);
        if (!vals[i]) {
            PRINT("Thread %d: failed to allocate memory for value %d", thread_id, i);
            free(keys[i]);  // Free the key if value allocation fails
            return NULL;
        }

        // Generate a unique key for this thread
        snprintf(keys[i], KEY_SIZE, "key_%d_%d", thread_id, i);
        *vals[i] = thread_id * KEY_COUNT + i;  // Unique value for each key
    }

    // Insert key-value pairs into the cache
    for (int i = 0; i < KEY_COUNT; i++) {
        int result = cache_put(keys[i], KEY_SIZE, vals[i], VAL_SIZE, cache);
        if (result != 0) {
            PRINT("Thread %d: failed to insert key-value pair: %s / %i", thread_id, keys[i], *vals[i]);
        }
        // PRINT("Thread %d: succedeed in inserting key-value pair: %s / %i", thread_id, keys[i], *vals[i]);
    }

    // Retrieve keys from the cache
    for (int i = 0; i < KEY_COUNT; i++) {
        LookupResult lr = cache_get(keys[i], KEY_SIZE, cache);

        if (lookup_result_is_ok(lr)) {
            int retrieved_value = *((int*)lookup_result_get_value(lr));
            if (retrieved_value != *vals[i]) {
                PRINT("Thread %d: GET mismatch for key %s: expected %i, got %i", thread_id, keys[i], *vals[i], retrieved_value);
            }

            // PRINT("Thread %d: got the pair %s / %i", thread_id, keys[i], retrieved_value);

        } else {
            PRINT("Thread %d: Failed to GET the key %s", thread_id, keys[i]);
        }
    }

    // Delete some keys (every 10th key)
    for (int i = 0; i < KEY_COUNT; i++) {
        if (i % 10 == 0) {
            int result = cache_delete(keys[i], KEY_SIZE, cache);
            if (result < 0) {
                PRINT("Thread %d: failed to delete key: %s", thread_id, keys[i]);
            }

            // PRINT("Thread %d: succeeded in deleting key at index %i", thread_id, i);
        }
    }

    PRINT("Thread %d: DELETION SUCCESSFUL", thread_id);

    // Retrieve keys again after deletion
    for (int i = 0; i < KEY_COUNT; i++) {
        if (i % 10 == 0) {
            LookupResult lr = cache_get(keys[i], KEY_SIZE, cache);

            if (lookup_result_is_ok(lr)) {
                PRINT("Thread %d: GET after deletion: (%s, %i) (should not happen!)", thread_id, keys[i], *((int*)lookup_result_get_value(lr)));
            } else {
                // PRINT("Thread %d: key was successfully not found after deletion.", thread_id);
            }

            
        }
    }

    // Free allocated memory
    for (int i = 0; i < KEY_COUNT; i++) {
        if (i % 10 == 0)
            continue;

        free(keys[i]);
        free(vals[i]);
    }

    return NULL;
}

int main() {
    setbuf(stdout, NULL);

    // Create the cache with a larger size to handle more entries
    Cache cache = cache_create(kr_hash);
    if (!cache) {
        PRINT("Failed to create cache");
        return 1;
    }

    // Number of threads to create (limited to 16)
    const int NUM_THREADS = 16;  // Respect the 16-thread limit
    pthread_t threads[NUM_THREADS];
    ThreadArgs thread_args[NUM_THREADS];

    // Create threads
    for (int i = 0; i < NUM_THREADS; i++) {
        thread_args[i].thread_id = i;
        thread_args[i].cache = cache;  // Cache is already a pointer, no need to dereference

        int result = pthread_create(&threads[i], NULL, thread_func, &thread_args[i]);
        if (result != 0) {
            PRINT("Failed to create thread %d", i);
            return 1;
        }
    }

    // Wait for all threads to finish
    for (int i = 0; i < NUM_THREADS; i++) {
        pthread_join(threads[i], NULL);
    }

    // Show cache statistics
    cache_stats(cache);

    return 0;
}
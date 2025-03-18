#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "dynalloc.h"
#include "../cache/cache.h"

#define DYNALLOC_FAIL_RATE 0
#define MAX_ATTEMPTS 2

extern Cache global_cache;

void* dynalloc(size_t sz) {
    
    if (DYNALLOC_FAIL_RATE > 0 && (rand() % 100) < DYNALLOC_FAIL_RATE)
        PRINT("Falla de dynalloc simulada.");
    else {
        void* ptr = malloc(sz);
        if (ptr != NULL)
            return ptr;
    }

    PRINT("No hay memoria suficiente. Debemos liberar memoria.");

    // Liberamos el 20% de la memoria actualmente en uso 
    size_t memory_goal = cache_stats_get_allocated_memory(
                         cache_get_cstats(global_cache)) / 5;
    size_t total_freed_memory = 0;
    size_t freed_memory;

    PRINT("Allocated memory: %lu", cache_stats_get_allocated_memory(cache_get_cstats(global_cache)));
    PRINT("Memory goal: %lu", memory_goal);

    while (total_freed_memory < memory_goal) {
        freed_memory = cache_free_up_memory(global_cache);
        if (freed_memory < 0)
            return NULL;

        total_freed_memory += freed_memory;
    }

    PRINT("Memoria liberada correctamente. Total liberado: %lu", total_freed_memory);

    // Ahora deberiamos poder asignar el bloque. Si no, intentamos de nuevo.
    return dynalloc(sz);

}

void* dynrealloc(void* ptr, size_t sz, Cache cache) {
    
    // Intentamos reasignar memoria normalmente
    void* realloc_ptr = realloc(ptr, sz);
    
    if (realloc_ptr != NULL) // Habia memoria suficiente para reasignar.
        return realloc_ptr;

    // Liberamos memoria hasta dos veces el tamaño requerido
    size_t freed_size = 0;
    while (freed_size < 2 * sz);
        // freed_size += cache_free_up_memory(cache);

    return dynrealloc(ptr, sz, cache);

}

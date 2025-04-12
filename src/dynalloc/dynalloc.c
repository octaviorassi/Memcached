#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "dynalloc.h"
#include "../cache/cache.h"

#define DYNALLOC_FAIL_RATE 0
#define MAX_ATTEMPTS 2

static size_t max(size_t goal, size_t sz) { return sz < goal ? goal : sz; }

void* dynalloc(size_t sz, Cache cache) {
    
    if (DYNALLOC_FAIL_RATE > 0 && (rand() % 100) < DYNALLOC_FAIL_RATE)
        PRINT("Falla de dynalloc simulada.");
    else {
        void* ptr = malloc(sz);
        if (ptr != NULL)
            return ptr;
    }

    PRINT("No hay memoria suficiente. Debemos liberar memoria.");

    // Liberaremos el maximo entre el 20% de la memoria ocupada actual y el size del bloque a asignar
    size_t memory_goal = max(cache_stats_get_allocated_memory(
                             cache_get_cstats(cache)) / 5,
                             sz);
    size_t total_freed_memory = 0;
    size_t freed_memory;

    PRINT("Allocated memory: %lu", cache_stats_get_allocated_memory(cache_get_cstats(cache)));
    PRINT("Memory goal: %lu", memory_goal);

    while (total_freed_memory < memory_goal) {

        freed_memory = cache_free_up_memory(cache);

        // Si se produjo algun error, directamente retornamos NULL
        if (freed_memory < 0)
            return NULL;

        total_freed_memory += freed_memory;
    }

    PRINT("Memoria liberada correctamente. Total liberado: %lu", total_freed_memory);

    // Ahora deberiamos poder asignar el bloque siempre.
    return malloc(sz);

}

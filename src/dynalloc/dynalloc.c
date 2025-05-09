#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "dynalloc.h"
#include "../cache/cache.h"

#define DYNALLOC_FAIL_RATE 0
#define MAX_ATTEMPTS 2
#define SIZE_GOAL_FACTOR 2

static size_t max(size_t goal, size_t sz) { return sz < goal ? goal : sz; }

void* dynalloc(size_t sz, Cache cache) {
    
    if (DYNALLOC_FAIL_RATE > 0 && (rand() % 100) < DYNALLOC_FAIL_RATE)
        PRINT("Falla de dynalloc simulada.");
    else {
        void* ptr = malloc(sz);
        if (ptr != NULL)
            return ptr;
    }


    // Liberaremos el maximo entre el 20% de la memoria ocupada actual y el size del bloque a asignar por un size_factor
    size_t memory_goal = max(cache_stats_get_allocated_memory(
                             cache_get_cstats(cache)) / 5,
                             sz * SIZE_GOAL_FACTOR);
    size_t total_freed_memory = 0;
    ssize_t freed_memory;

    int attempts = 0;

    while (total_freed_memory < memory_goal && attempts < MAX_ATTEMPTS) {
                
        freed_memory = cache_free_up_memory(cache, memory_goal - total_freed_memory);

        // Si se produjo algun error, directamente retornamos NULL
        if (freed_memory < 0)
            return NULL;

        if (freed_memory == 0) {
            sched_yield();
            attempts++;
        }

        total_freed_memory += freed_memory;
    }

    // Tanto si se logro el objetivo de memoria como si se agotaron los intentos, devolvemos directamente malloc.
    // Si no alcanza la memoria, aceptamos que devuelva NULL pues la cache debe estar sobrecargada de pedidos. 
    return malloc(sz);

}

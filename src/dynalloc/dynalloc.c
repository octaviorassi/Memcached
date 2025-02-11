#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "dynalloc.h"
#include "../cache/cache.h"

#define DYNALLOC_FAIL_RATE 10
#define MAX_ATTEMPTS 2

void* dynalloc(size_t sz, Cache cache) {
    // Intentamos asignar memoria normalmente
    if (DYNALLOC_FAIL_RATE > 0 && (rand() % 100) < DYNALLOC_FAIL_RATE) {

        PRINT("Falla de DYNALLOC simulada. Debemos liberar memoria");

    } else {

        void* ptr = malloc(sz);
        if (ptr != NULL)
            return ptr;
        
        PRINT("No hay memoria suficiente. Debemos liberar memoria.");

    }

    // Liberamos memoria.
    size_t total_freed = 0;
    size_t freed_size = 0;
    int attempts = 0;

    while (total_freed < 2 * sz && attempts < MAX_ATTEMPTS) {
        freed_size = cache_free_up_memory(cache);
        total_freed += freed_size;

        if (freed_size == 0) attempts++;
    }

    if (attempts == MAX_ATTEMPTS) {
        PRINT("Maximo de intentos de desalojo alcanzado.");
        void* ptr = malloc(sz);
        if (ptr)
            return ptr;
    }

    // Ahora deberiamos poder asignar el bloque. Si no, intentamos de nuevo.
    return dynalloc(sz, cache);

}

void* dynrealloc(void* ptr, size_t sz, Cache cache) {
    
    // Intentamos reasignar memoria normalmente
    void* realloc_ptr = realloc(ptr, sz);
    
    if (realloc_ptr != NULL) // Habia memoria suficiente para reasignar.
        return realloc_ptr;

    // Liberamos memoria hasta dos veces el tamaño requerido
    size_t freed_size = 0;
    while (freed_size < 2 * sz)
        freed_size += cache_free_up_memory(cache);

    return dynrealloc(ptr, sz, cache);

}

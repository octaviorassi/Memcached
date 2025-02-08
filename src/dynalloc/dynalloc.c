#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "dynalloc.h"
#include "../cache/cache.h"

void* dynalloc(size_t sz, Cache cache) {

    // Intentamos asignar memoria normalmente
    void* ptr = malloc(sz);
    if (ptr != NULL)
        return ptr;

    // Liberamos memoria.
    size_t freed_size = 0;
    while (freed_size < 2 * sz)
        freed_size += cache_free_up_memory(cache);

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

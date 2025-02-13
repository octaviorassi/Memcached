#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "dynalloc.h"
#include "../cache/cache.h"

#define DYNALLOC_FAIL_RATE 10
#define MAX_ATTEMPTS 2

void* dynalloc(size_t sz, Cache cache) {
    

    void* ptr = malloc(sz);
    if (ptr != NULL)
        return ptr;
    
    PRINT("No hay memoria suficiente. Debemos liberar memoria.");

    // Liberamos memoria.
    int num_nodes_to_eliminate = 2;
    int num_nodes_eliminated = 0;
    int num_nodes_eliminated_acum = 0;

    while (num_nodes_eliminated_acum < num_nodes_to_eliminate) {

        num_nodes_eliminated = cache_free_up_memory(cache, num_nodes_to_eliminate - num_nodes_eliminated_acum);
        
        if (num_nodes_eliminated < 0) return NULL;

        num_nodes_eliminated_acum += num_nodes_eliminated;
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
    while (freed_size < 2 * sz);
        // freed_size += cache_free_up_memory(cache);

    return dynrealloc(ptr, sz, cache);

}

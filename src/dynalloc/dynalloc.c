#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "dynalloc.h"
#include "../cache/cache.h"

void* dynalloc(size_t sz, pthread_mutex_t* lock, Cache cache) {

    // Intentamos asignar memoria normalmente
    void* ptr = malloc(sz);
    if (ptr != NULL)
        return ptr;

    // Si falla, comenzamos soltando nuestro lock en caso de poseer uno.
    if (lock)
        pthread_mutex_unlock(lock);

    // ! Esto es horrible, lo sacamos por ahora y vemos como reemplazarlo.
    // ! No siempre que llame voy a tener un bucket_number.
    // cache_unlock_zone_mutex(bucket_number, cache);

    // Lockeamos toda la cache
    cache_lock_all_zones(cache);

    // Liberamos memoria.
    // todo: que libere un % de la memoria de la cache.
    size_t freed_size = 0;
    while (freed_size < 2 * sz)
        freed_size += cache_free_up_memory(cache);

    // Ahora deberiamos poder asignar el bloque.
    ptr = malloc(sz);

    // Liberamos la cache.
    cache_unlock_all_zones(cache);    

    // Chequeamos que efectivamente se haya podido asignar memoria. Caso contrario, volvemos a llamar a dynalloc.
    if (ptr == NULL) {
        if (lock)
            pthread_mutex_lock(lock);
        return dynalloc(sz, lock, cache);
    }
    
    // Readquirimos el mutex inicial y retornamos el puntero al bloque.
    if (lock)
        pthread_mutex_lock(lock);

    return ptr;

}

void* dynrealloc(void* ptr, size_t sz, pthread_mutex_t* lock, Cache cache) {
    
    // Intentamos reasignar memoria normalmente
    void* realloc_ptr = realloc(ptr, sz);
    
    if (realloc_ptr != NULL) // Habia memoria suficiente para reasignar.
        return realloc_ptr;


    // No habia suficiente memoria para todo el bloque
    // Largamos nuestro mutex y lockeamos toda la cache.
    if (lock)
        pthread_mutex_unlock(lock);

    cache_lock_all_zones(cache);

    // Liberamos memoria hasta dos veces el tamaño requerido
    size_t freed_size = 0;
    while (freed_size < 2 * sz)
        freed_size += cache_free_up_memory(cache);

    // Intentamos reasignar nuevamente y liberamos la cache, readquiriendo el lock inicial
    realloc_ptr = realloc(ptr, sz);

    cache_unlock_all_zones(cache);

    if (lock)
        pthread_mutex_lock(lock);

    // Chequeamos que se haya podido reasignar la memoria.
    if (realloc_ptr == NULL)
        return dynrealloc(ptr, sz, lock, cache);

    return realloc_ptr;

}

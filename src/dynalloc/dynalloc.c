#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "dynalloc.h"
#include "../cache/cache.h"

// asi dynalloc conoce a la cache
void* dynalloc(size_t sz, Cache cache) {

    // Intentamos asignar memoria normalmente
    void* ptr = malloc(sz);
    if (ptr != NULL)
        return ptr;

    // Si falla, comenzamos soltando nuestro lock.
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
    if (ptr == NULL)
        return dynalloc(sz, cache);
    
    // Readquirimos el mutex inicial y retornamos el puntero al bloque.
    // ! idem lo sacamos por ahora
    // cache_lock_zone_mutex(bucket_number, cache);
    
    return ptr;

}

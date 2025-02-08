#include <stdlib.h>
#include <stdio.h>
#include "cache_stats.h"

struct CacheStats {

    AtomCounter put_counter;
    AtomCounter get_counter;
    AtomCounter del_counter;

    AtomCounter key_counter;

};


CacheStats cache_stats_create() {
    
    CacheStats stats = malloc(sizeof(struct CacheStats));
    if (stats == NULL)
        return NULL;

    // Aca no chequeo que se inicialicen
    stats->put_counter = atom_counter_create(0);
    stats->get_counter = atom_counter_create(0);
    stats->del_counter = atom_counter_create(0);

    stats->key_counter = atom_counter_create(0);

    return stats;

}


int cache_stats_put_counter_inc(CacheStats cstats) {
    return atom_counter_inc(cstats->put_counter);
}

int cache_stats_put_counter_dec(CacheStats cstats) {
    return atom_counter_dec(cstats->put_counter);
}


int cache_stats_get_counter_inc(CacheStats cstats) {
    return atom_counter_inc(cstats->get_counter);
}

int cache_stats_get_counter_dec(CacheStats cstats) {
    return atom_counter_dec(cstats->get_counter);
}


int cache_stats_del_counter_inc(CacheStats cstats) {
    return atom_counter_inc(cstats->get_counter);
}

int cache_stats_del_counter_dec(CacheStats cstats) {
    return atom_counter_dec(cstats->del_counter);
}


int cache_stats_key_counter_inc(CacheStats cstats) {
    return atom_counter_inc(cstats->key_counter);
}

int cache_stats_key_counter_dec(CacheStats cstats) {
    return atom_counter_dec(cstats->key_counter);
}


void cache_stats_show(CacheStats cstats, char* buf) { 
   
    // Por ahora imprime en pantalla unicamente.

    printf("******************* CACHE STATS *******************\n");
    printf("PUTS: %u\n", atom_counter_get(cstats->put_counter));
    printf("GETS: %u\n", atom_counter_get(cstats->get_counter));
    printf("DELS: %u\n", atom_counter_get(cstats->del_counter));

    printf("KEYS: %u\n", atom_counter_get(cstats->key_counter));
    printf("***************************************************\n");

    return;
    
}


int cache_stats_destroy(CacheStats cstats) {

    if (cstats == NULL)
        return 0;
    
    atom_counter_destroy(cstats->put_counter);
    atom_counter_destroy(cstats->get_counter);
    atom_counter_destroy(cstats->del_counter);
    atom_counter_destroy(cstats->key_counter);

    free(cstats);

    return 0;

}
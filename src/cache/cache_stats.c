#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "cache_stats.h"

struct CacheStats {

    AtomCounter put_counter;
    AtomCounter get_counter;
    AtomCounter del_counter;

    AtomCounter evict_counter;
    AtomCounter key_counter;

    AtomCounter allocated_memory;

};


CacheStats cache_stats_create() {
    
    CacheStats stats = malloc(sizeof(struct CacheStats));
    if (stats == NULL)
        return NULL;

    stats->put_counter = atom_counter_create(0);
    stats->get_counter = atom_counter_create(0);
    stats->del_counter = atom_counter_create(0);

    stats->evict_counter = atom_counter_create(0);
    stats->key_counter   = atom_counter_create(0);
    stats->allocated_memory = atom_counter_create(0);

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
    return atom_counter_inc(cstats->del_counter);
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


int cache_stats_evict_counter_inc(CacheStats cstats) {
    return atom_counter_inc(cstats->evict_counter);
}

int cache_stats_evict_counter_dec(CacheStats cstats) {
    return atom_counter_dec(cstats->evict_counter);
}

int cache_stats_allocated_memory_add(CacheStats cstats, Counter mem) {
    return atom_counter_add(cstats->allocated_memory, mem);
}

int cache_stats_allocated_memory_free(CacheStats cstats, Counter mem) {
    return atom_counter_drop(cstats->allocated_memory, mem);
}

Counter cache_stats_get_allocated_memory(CacheStats cstats) {
    return atom_counter_get(cstats->allocated_memory);
}

StatsReport cache_stats_report(CacheStats cstats) {

    StatsReport report;
    
    report.put   = atom_counter_get(cstats->put_counter);
    report.get   = atom_counter_get(cstats->get_counter);
    report.del   = atom_counter_get(cstats->del_counter);
    report.key   = atom_counter_get(cstats->key_counter);
    report.evict = atom_counter_get(cstats->evict_counter);
    report.allocated_memory = atom_counter_get(cstats->allocated_memory);

    return report;

}




void cache_stats_show(CacheStats cstats, char* buf) { 
   
    printf("****************** CACHE STATS *******************\n");
    printf("PUTS: " COUNTER_FORMAT "\n", atom_counter_get(cstats->put_counter));
    printf("GETS: " COUNTER_FORMAT "\n", atom_counter_get(cstats->get_counter));
    printf("DELS: " COUNTER_FORMAT "\n", atom_counter_get(cstats->del_counter));
    printf("KEYS: " COUNTER_FORMAT "\n", atom_counter_get(cstats->key_counter));
    
    printf("EVICTS: " COUNTER_FORMAT "\n",
            atom_counter_get(cstats->evict_counter));

    printf("ALLOCATED MEMORY: " COUNTER_FORMAT "\n",
            atom_counter_get(cstats->allocated_memory));

    printf("**************************************************\n");

    return;
    
}


int cache_stats_destroy(CacheStats cstats) {

    if (cstats == NULL)
        return 1;
    
    atom_counter_destroy(cstats->put_counter);
    atom_counter_destroy(cstats->get_counter);
    atom_counter_destroy(cstats->del_counter);

    atom_counter_destroy(cstats->evict_counter);
    atom_counter_destroy(cstats->key_counter);

    free(cstats);

    return 0;

}
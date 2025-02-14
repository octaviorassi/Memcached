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

int cache_stats_evictcounter_dec(CacheStats cstats) {
    return atom_counter_dec(cstats->evict_counter);
}


StatsReport cache_stats_report(CacheStats cstats) {

    StatsReport report;
    
    report.put   = atom_counter_get(cstats->put_counter);
    report.get   = atom_counter_get(cstats->get_counter);
    report.del   = atom_counter_get(cstats->del_counter);
    report.key   = atom_counter_get(cstats->key_counter);
    report.evict = atom_counter_get(cstats->evict_counter);

    return report;

}

void cache_stats_show(CacheStats cstats, char* buf) { 
   
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

    atom_counter_destroy(cstats->evict_counter);
    atom_counter_destroy(cstats->key_counter);

    free(cstats);

    return 0;

}
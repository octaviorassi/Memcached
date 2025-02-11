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

    // Aca no chequeo que se inicialicen
    stats->put_counter = atom_counter_create(0);
    stats->get_counter = atom_counter_create(0);
    stats->del_counter = atom_counter_create(0);

    stats->evict_counter = atom_counter_create(0);
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


void cache_stats_show2(CacheStats cstats, char* buf) { 
   
    // Por ahora imprime en pantalla unicamente.

    printf("******************* CACHE STATS *******************\n");
    printf("PUTS: %u\n", atom_counter_get(cstats->put_counter));
    printf("GETS: %u\n", atom_counter_get(cstats->get_counter));
    printf("DELS: %u\n", atom_counter_get(cstats->del_counter));

    printf("KEYS: %u\n", atom_counter_get(cstats->key_counter));
    printf("***************************************************\n");

    return;
    
}
void cache_stats_show(CacheStats cstats, char* buf) {
    // Calculate the maximum line length for the box
    char temp[256];
    int max_length = 0;

    // Generate each line of stats and check its length
    snprintf(temp, sizeof(temp), "PUTS: %u", atom_counter_get(cstats->put_counter));
    if (strlen(temp) > max_length) max_length = strlen(temp);

    snprintf(temp, sizeof(temp), "GETS: %u", atom_counter_get(cstats->get_counter));
    if (strlen(temp) > max_length) max_length = strlen(temp);

    snprintf(temp, sizeof(temp), "DELS: %u", atom_counter_get(cstats->del_counter));
    if (strlen(temp) > max_length) max_length = strlen(temp);

    snprintf(temp, sizeof(temp), "KEYS: %u", atom_counter_get(cstats->key_counter));
    if (strlen(temp) > max_length) max_length = strlen(temp);

    // Add padding for the box borders and margins
    // Each line has 2 spaces on the left and 1 space on the right, plus the borders
    int box_width = max_length + 4;  // 2 spaces + 2 borders

    // Print the top border of the box
    printf("\n\n╔");
    for (int i = 0; i < box_width; i++) printf("═");
    printf("╗\n");

    // Print the title
    printf("║ %-*s  ║\n", max_length, "CACHE STATS");

    // Print a separator line
    printf("╠");
    for (int i = 0; i < box_width; i++) printf("═");
    printf("╣\n");

    // Print the stats
    printf("║ PUTS: %-*u  ║\n", max_length - 5, atom_counter_get(cstats->put_counter));
    printf("║ GETS: %-*u  ║\n", max_length - 5, atom_counter_get(cstats->get_counter));
    printf("║ DELS: %-*u  ║\n", max_length - 5, atom_counter_get(cstats->del_counter));
    
    printf("║ EVICTS: %-*u║\n", max_length - 5, atom_counter_get(cstats->evict_counter));
    printf("║ KEYS: %-*u  ║\n", max_length - 5, atom_counter_get(cstats->key_counter));


    // Print the bottom border of the box
    printf("╚");
    for (int i = 0; i < box_width; i++) printf("═");
    printf("╝\n\n");
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
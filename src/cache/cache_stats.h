#ifndef __CACHE_STATS__
#define __CACHE_STATS__

#include "../helpers/atom_counter.h"

typedef struct CacheStats* CacheStats;

CacheStats cache_stats_create();

int cache_stats_put_counter_inc(CacheStats cstats);
int cache_stats_put_counter_dec(CacheStats cstats);

int cache_stats_get_counter_inc(CacheStats cstats);
int cache_stats_get_counter_dec(CacheStats cstats);

int cache_stats_del_counter_inc(CacheStats cstats);
int cache_stats_del_counter_dec(CacheStats cstats);

int cache_stats_evict_counter_inc(CacheStats cstats);
int cache_stats_evict_counter_dec(CacheStats cstats);

int cache_stats_key_counter_inc(CacheStats cstats);
int cache_stats_key_counter_dec(CacheStats cstats);

void cache_stats_show(CacheStats cstats, char* buf);

int cache_stats_destroy(CacheStats cstats);



#endif // __CACHE_STATS__
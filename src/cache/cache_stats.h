#ifndef __CACHE_STATS__
#define __CACHE_STATS__

#include "../helpers/atom_counter.h"

typedef struct CacheStats* CacheStats;
typedef struct StatsReport StatsReport;

/**
 *  @brief Crea un nuevo `CacheStats` con todos sus contadores de operaciones inicializados a 0.
 *  @return El nuevo struct CacheStats.
 */
CacheStats cache_stats_create();

/**
 *  @brief Incrementa el contador de operaciones `PUT` del acumulador de estadisticas `cstats`.
 * 
 *  @param cstats El acumulador de estadisticas objetivo.
 *  @return 0 si la operacion es exitosa, -1 si se produjo un error.
 */
int cache_stats_put_counter_inc(CacheStats cstats);

/**
 *  @brief Decrementa el contador de operaciones `PUT` del acumulador de estadisticas `cstats`.
 * 
 *  @param cstats El acumulador de estadisticas objetivo.
 *  @return 0 si la operacion es exitosa, -1 si se produjo un error.
 */
int cache_stats_put_counter_dec(CacheStats cstats);

/**
 *  @brief Incrementa el contador de operaciones `GET` del acumulador de estadisticas `cstats`.
 * 
 *  @param cstats El acumulador de estadisticas objetivo.
 *  @return 0 si la operacion es exitosa, -1 si se produjo un error.
 */
int cache_stats_get_counter_inc(CacheStats cstats);

/**
 *  @brief Decrementa el contador de operaciones `GET` del acumulador de estadisticas `cstats`.
 * 
 *  @param cstats El acumulador de estadisticas objetivo.
 *  @return 0 si la operacion es exitosa, -1 si se produjo un error.
 */
int cache_stats_get_counter_dec(CacheStats cstats);

/**
 *  @brief Incrementa el contador de operaciones `DEL` del acumulador de estadisticas `cstats`.
 * 
 *  @param cstats El acumulador de estadisticas objetivo.
 *  @return 0 si la operacion es exitosa, -1 si se produjo un error.
 */
int cache_stats_del_counter_inc(CacheStats cstats);

/**
 *  @brief Decrementa el contador de operaciones `DEL` del acumulador de estadisticas `cstats`.
 * 
 *  @param cstats El acumulador de estadisticas objetivo.
 *  @return 0 si la operacion es exitosa, -1 si se produjo un error.
 */
int cache_stats_del_counter_dec(CacheStats cstats);

/**
 *  @brief Incrementa el contador de operaciones de expulsion (`evict`) del  producto de la politica de desalojo implementada.
 * 
 *  @param cstats El acumulador de estadisticas objetivo.
 *  @return 0 si la operacion es exitosa, -1 si se produjo un error.
 */
int cache_stats_evict_counter_inc(CacheStats cstats);

/**
 *  @brief Decrementa el contador de operaciones de expulsion (`evict`) del  producto de la politica de desalojo implementada.
 * 
 *  @param cstats El acumulador de estadisticas objetivo.
 *  @return 0 si la operacion es exitosa, -1 si se produjo un error.
 */
int cache_stats_evict_counter_dec(CacheStats cstats);

/**
 *  @brief Incrementa el contador de claves del acumulador de estadisticas `cstats`.
 * 
 *  @param cstats El acumulador de estadisticas objetivo.
 *  @return 0 si la operacion es exitosa, -1 si se produjo un error.
 */
int cache_stats_key_counter_inc(CacheStats cstats);

/**
 *  @brief Decrementa el contador de claves del acumulador de estadisticas `cstats`.
 * 
 *  @param cstats El acumulador de estadisticas objetivo.
 *  @return 0 si la operacion es exitosa, -1 si se produjo un error.
 */
int cache_stats_key_counter_dec(CacheStats cstats);

/**
 *  @brief Genera un StatsReport con informacion sobre las metricas de la cache al momento de ser invocada.
 * 
 *  @param cstats El CacheStats asociado a la cache de interes.
 *  @return Un StatsReport con metricas de la cache. 
 */
StatsReport cache_stats_report(CacheStats cstats);

void cache_stats_show(CacheStats cstats, char* buf);

int cache_stats_destroy(CacheStats cstats);



#endif // __CACHE_STATS__
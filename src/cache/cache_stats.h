#ifndef __CACHE_STATS_H__
#define __CACHE_STATS_H__

#include "../helpers/atom_counter.h"

#define STATS_COUNT 6 // Es la cantidad de estadisticos que reportamos.

typedef struct CacheStats* CacheStats;

typedef struct StatsReport {
    Counter put;
    Counter get;
    Counter del;
    Counter key;
    Counter evict;
    Counter allocated_memory;
} StatsReport;

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
 *  @brief Aumenta el acumulador de memoria asignada de la cache en `mem`.
 * 
 *  @param cstats Puntero a la estructura CacheStats con informacion de la cache.
 *  @param mem Cantidad de memoria a incrementar.
 * 
 *  @return 0 si la operacion es exitosa, -1 si se produjo un error.
 */
int cache_stats_allocated_memory_add(CacheStats cstats, Counter mem);


/**
 *  @brief Substrae `rem` del acumulador de memoria asignada de la cache.
 * 
 *  @param cstats Puntero a la estructura CacheStats con informacion de la cache.
 *  @param mem Cantidad de memoria a sustraer.
 * 
 *  @return 0 si la operacion es exitosa, -1 si se produjo un error.
 */
int cache_stats_allocated_memory_free(CacheStats cstats, Counter mem);

/**
 *  @brief Obtiene la cantidad de memoria asignada dinamicamente para claves y valores de la cache asociada a `cstats`.abort
 *  
 *  @param cstats Puntero a la estructura CacheStats con informacion de la cache.
 *  @return La cantidad de memoria asignada dinamicamente a claves y valores.
 */
Counter cache_stats_get_allocated_memory(CacheStats cstats);



/**
 *  @brief Genera un StatsReport con informacion sobre las metricas de la cache al momento de ser invocada.
 * 
 *  @param cstats El CacheStats asociado a la cache de interes.
 *  @return Un StatsReport con metricas de la cache. 
 */
StatsReport cache_stats_report(CacheStats cstats);

void cache_stats_show(CacheStats cstats, char* buf);


/**
 *  @brief Destruye la estructura CacheStats apuntada por `cstats`, liberando la memoria que se le habia asignado.
 * 
 *  @param cstats Puntero a la estructura CacheStats a destruir.
 *  @return 0 en caso de liberar exitosamente, 1 si el puntero era NULL.
 */
int cache_stats_destroy(CacheStats cstats);



#endif // __CACHE_STATS_H__
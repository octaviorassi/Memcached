#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include "hash.h"
#include "../helpers/results.h"

#define N_LOCKS 10

struct HashMap {

    HashFunction        hash_function;

    Bucket*             buckets;
    int                 n_buckets;
    pthread_mutex_t*    zone_locks[N_LOCKS];

};

// ! podria ser unsigned int, pero quiero poder devolver -1 por si falla algo
// ! tambien podria obviar esta funcion y hacer directamente una que devuelva el puntero al bucket correspondiente
// ! pero en algun momento voy a querer saber el bucket_number para calcular que numero de lock tomar
int hashmap_find_bucket_number(int key, HashMap map) {

    if (map == NULL)
        return -1;

    return map->hash_function(key) % map->n_buckets;

}

Bucket hashmap_find_bucket(int bucket_number, HashMap map) {
    
    if (map == NULL || map->buckets == NULL)
        return NULL;

    return bucket_number < 0 ? NULL : map->buckets[bucket_number];

}

// todo, creo que a esta no la vamos a querer exportar.
pthread_mutex_t* hashmap_get_zone_mutex(int bucket_number, HashMap map) {
    return NULL;
}

HashNode hashmap_insert(int key, int val, HashMap map) { 

    HashNode node = lru_hash_node_create(key, val);

    if (node == NULL)
        return NULL;

    int bucket_number = hashmap_find_bucket_number(key, map);
    
    // ? Tiene sentido siquiera chequear esto? Puede en algun caso la funcion de hash fallar?
    if (bucket_number < 0) {
        lru_hash_node_destroy(node);
        return NULL;
    }

    // Tomamos el mutex
    pthread_mutex_t* zone_mutex = hashmap_get_zone_mutex(bucket_number, map);
    pthread_mutex_lock(zone_mutex);

    // ! No hace falta chequear que bucket_number < map->n_buckets porque hashmap_find_bucket lo garantiza
    // ! Tampoco deberia hacer falta chequear que buckets != NULL si asumimos que hashmap_init lo crea correctamente.
    Bucket bucket = map->buckets[bucket_number];
    
    /*  Insertamos:
            I.   El prev del bucket pasa a ser node
            II.  El next del node   pasa a ser bucket
            III. El bucket pasa a ser node
    */

    lru_hash_node_set_hash_prev(bucket, node);
    lru_hash_node_set_hash_next(node, bucket);
    map->buckets[bucket_number] = node;

    // Liberamos el mutex de zona
    pthread_mutex_unlock(zone_mutex);

    return node;

}

LookupResult hashmap_lookup(int key, HashMap map) { 

    // !!! No chequeo que map != NULL, porque si map == NULL no se como avisar, ya que no hay un int que pueda devoleer
    if (map == NULL)
        return create_error_lookup_result();

    int bucket_number = hashmap_find_bucket_number(key, map);

    if (bucket_number < 0)
       return create_error_lookup_result();

    return lru_hash_node_lookup(key, hashmap_find_bucket(bucket_number, map));

}

// todo
int hashmap_delete(int key, HashMap map) { return 0; }

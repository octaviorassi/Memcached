#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hashmap.h"
#include "hashnode.h"
#include "../helpers/results.h"

#define N_LOCKS 10

struct HashMap {

    HashFunction        hash_function;

    HashNode*           buckets;
    int                 n_buckets;
    pthread_mutex_t*    zone_locks[N_LOCKS];  

};



HashMap hashmap_create(HashFunction hash, int n_buckets) {

    HashMap map = malloc(sizeof(struct HashMap));

    if (map == NULL)
        return NULL;

    memset(map, 0, sizeof(struct HashMap));
    
    map->hash_function = hash;

    // Asignamos memoria para los buckets, no hace falta inicializarlos, el insert lo hara.
    HashNode* buckets = malloc(sizeof(HashNode) * n_buckets);
    if (buckets == NULL)
        return NULL;

    map->n_buckets = n_buckets;

    // Inicializamos los locks
    int mutex_error = 0;
    for (int i = 0; i < N_LOCKS; i++) 
        mutex_error = mutex_error || pthread_mutex_init(map->zone_locks[i], NULL);

    if (mutex_error)
        return NULL;

    return map;

}

HashNode hashmap_insert(int key, int val, HashMap map) { 

    HashNode node = hashnode_create(key, val);

    if (node == NULL)
        return NULL;


    int bucket_number = hashmap_get_bucket_number(key, map);

    HashNode bucket = map->buckets[bucket_number];
    
    /* Insercion:
            I.   El prev del bucket pasa a ser node
            II.  El next del node   pasa a ser bucket
            III. El bucket pasa a ser node
    */

    hashnode_set_prev(bucket, node);
    hashnode_set_next(node, bucket);
    map->buckets[bucket_number] = node;

    return node;

}

HashNode hashmap_update(int key, int val, HashMap map) { 

    HashNode node = hashmap_lookup_node(key, map);

    // La clave no pertenecia a ningun par
    if (node == NULL)
        return NULL;

    // La clave estaba asociada a un par
    hashnode_set_val(node, val);

    // ! Aca esta el problema. Deberia devolver el mutex ahora,
    // ! pero inmediatamente despues de llamar a hashmap_update
    // ! es probable que la Cache quiera hacer un lru_update
    // ! que requiere del mismo mutex. Entonces, conviene no
    // ! devolverlo, y que hashmap_update se quede el mutex
    // ! si logra actualizarlo.

    return node;

}

LookupResult hashmap_lookup(int key, HashMap map) { 

    if (map == NULL)
        return create_error_lookup_result();

    int bucket_number = hashmap_get_bucket_number(key, map);

    if (bucket_number < 0)
       return create_error_lookup_result();

    HashNode bucket = hashmap_get_bucket(bucket_number, map);
    LookupResult lr = hashnode_lookup(key, bucket);

    hashmap_unlock_zone_mutex(bucket_number, map);

    return lr;

}

HashNode hashmap_lookup_node(int key, HashMap map) {

    if (map == NULL)
        return NULL;

    int bucket_number = hashmap_get_bucket_number(key, map);

    if (bucket_number < 0)
       return NULL;

    /*  Aca obtenemos el mutex ademas del bucket, y no lo liberamos,
        pues se preserva tras el llamado a funcion. */
    HashNode bucket = hashmap_get_bucket(bucket_number, map);

    HashNode node = hashnode_lookup_node(key, bucket);

    return node;
}


// helpers

int hashmap_get_bucket_number(int key, HashMap map) {
    if (map == NULL)
        return -1;
    return map->hash_function(key) % map->n_buckets;
}

// ? podria devolver el bucket_number en caso exitoso, para ahorrar volver a calcularlo.
int hashmap_get_key_lock(int key, HashMap map) {

    if (map == NULL)
        return -1;
    
    int bucket_number = hashmap_get_bucket_number(key, map);

    pthread_mutex_t* lock = hashmap_get_zone_mutex(bucket_number, map);
    
    if (pthread_mutex_lock(lock) != 0)
        return -1;

    return 0;
}

int hashmap_release_key_lock(int key, HashMap map) {

    if (map == NULL)
        return -1;
    
    int bucket_number = hashmap_get_bucket_number(key, map);

    pthread_mutex_t* lock = hashmap_get_zone_mutex(bucket_number, map);
    
    if (pthread_mutex_unlock(lock) != 0)
        return -1;

    return 0;    

}

HashNode hashmap_get_bucket(int bucket_number, HashMap map) {
    
    if (map == NULL || map->buckets == NULL || bucket_number < 0)
        return NULL;

    // Tomo el mutex de la zona a la que pertenece el bucket
    if (hashmap_lock_zone_mutex(bucket_number, map) != 0)
      return NULL;

    return map->buckets[bucket_number];

}

// todo, creo que a estas no la vamos a querer exportar.
int hashmap_lock_zone_mutex(int bucket_number, HashMap map) {
    return 0;
}

int hashmap_unlock_zone_mutex(int bucket_number, HashMap map) {
    return 0;
}

// todo
int hashmap_delete_node(int key, HashMap map) { 

    // ! SETTEAR EL NODE A NULL ANTE DE HACER FREE

    return 0;

}

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hash.h"
#include "../helpers/results.h"

#define N_LOCKS 10

// ? HashNode es literalmente un LRUHashNode. Tiene sentido rehacer la interfaz de LRUHashNode pero llamandola
// ? solo HashNode? 
// ? Por ejemplo
// ?    int hashnode_get_value(HashNode node) { return lru_hash_node_get_val(node); }

struct HashMap {

    HashFunction        hash_function;

    Bucket*             buckets;
    int                 n_buckets;
    pthread_mutex_t*    zone_locks[N_LOCKS];  

    // Obs: la semantica de los locks es que protegen a todos los campos menos los relacionados a la lru
    // es decir, si tengo el lock sobre un bucket, puedo modificar key, val, hash_prev, hash_next.
    // Pero ademas, es ese mismo lock el que te habilita a deletear; la LRU no tiene derecho a eliminar nodos,
    // solo puede alterar la informacion de sus campos lru_prev y lru_next.
    // Por eso, la semantica de tomar un lock de LRU es poder modificar esos campos y NADA MAS.

};

// todo: ver mejor que argumentos pasar/son necesarios
HashMap hashmap_create(HashFunction hash, int n_buckets) {

    HashMap map = malloc(sizeof(struct HashMap));

    if (map == NULL)
        return NULL;

    memset(map, 0, sizeof(struct HashMap));
    
    map->hash_function = hash;

    // Asigno memoria para los buckets, no hace falta inicializarlos, el insert lo hara.
    // ! Chequear si no hay un tema con la memoria:
    // ! En el HashMap asigno memoria para n_buckets punteros a struct LRUHashNode
    // ! Es decir, n_buckets * sizeof(puntero) bytes
    // ! Despues cuando inserto, asigno memoria para un struct LRUHashNode (el STRUCT, no el puntero)
    // ! y ahi manipulo los punteros.
    Bucket* buckets = malloc(sizeof(Bucket) * n_buckets);
    if (buckets == NULL)
        return NULL;

    map->n_buckets = n_buckets;
    // ? memset a 0 de buckets es necesario?

    // Inicializo los locks
    int mutex_error = 0;
    for (int i = 0; i < N_LOCKS; i++) 
        mutex_error = mutex_error || pthread_mutex_init(map->zone_locks[i], NULL);
        // ? el cortocircuito del OR haria que no se evalue el init? seria buenisimo

    // no se si me gusta mas este ternario o hacer un if mas chequeando mutex_error y listo
    return mutex_error ? NULL : map;
}

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

    // !!!!!! Aca hace falta pedir el lock
    /* pthread_mutex_t* bucket_mutex = hashmap_get_zone_mutex(bucket_number, map);
    if (pthread_mutex_lock(bucket_mutex) != 0)
      return NULL;
    */

    // ! Aca podria haber un race condition?
    // ! Al desreferenciar map->buckets[bucket_number]
    // ! puede que:
    // !  1. Ya no sea el inicio del bucket
    // !  2. Sea el inicio, pero entre que es devuelto y es usado, otro hilo lo deletea, volviendolo NULL.
    
    // !  (1) No es tan grave, porque siempre una vez pedido el bucket la busqueda es lineal hacia adelante
    // !  luego si el bucket cambio, es porque se inserto un nuevo dato, y ese dato no deberia ser visible
    // !  para el proceso que pidio el bucket antes de todas formas.
    // !  (2) Si es grave, pero ademas pedir el lock no resuelve el problema tampoco. En todo caso,
    // !  esta funcion deberia quedar obsoleta, y hacer tanto el pedido del bucket como el lookup dentro
    // !  de una misma funcion.

    // !  SI NO, una posible solucion seria que hashmap_find_bucket tome el lock y no lo devuelva al retornar.
    // !  Es decir, que devuelva el bucket buscado y su lock tomado. Y si algo falla, libera el lock y devuelve NULL.
    return bucket_number < 0 ? NULL : map->buckets[bucket_number];

}

// todo, creo que a esta no la vamos a querer exportar.
pthread_mutex_t* hashmap_get_zone_mutex(int bucket_number, HashMap map) {
    return NULL;
}

// !!! ESTOY INSERTANDO SIN VER SI YA ESTABA.
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

    // ! revisar mutex
    return lru_hash_node_lookup(key, hashmap_find_bucket(bucket_number, map));

}

HashNode hashmap_lookup_node(int key, HashMap map) {

    if (map == NULL)
        return NULL;

    int bucket_number = hashmap_find_bucket_number(key, map);

    if (bucket_number < 0)
       return NULL;

    pthread_mutex_t* mutex = hashmap_get_zone_mutex(bucket_number, map);
    if (pthread_mutex_lock(mutex) != 0)
        return NULL;

    Bucket bucket = hashmap_find_bucket(bucket_number, map);

    HashNode node = lru_hash_node_lookup_node(key, bucket);

    if (pthread_mutex_unlock(mutex) != 0)
        return NULL;

    return node;
}

int hashmap_clean_node(HashNode node, HashMap map) {

  if (node == NULL || map == NULL)
    return -1;
  
  // Tomo el mutex
  int bucket_number = hashmap_find_bucket_number(lru_hash_node_get_key(node), map);

  if (bucket_number < 0)
    return -1;
  
  pthread_mutex_t* mutex = hashmap_get_zone_mutex(bucket_number, map);
  if (pthread_mutex_lock(mutex) != 0)
    return -1;

  // Desconectamos y reconectamos los adyacentes
  HashNode prev = lru_hash_node_get_hash_prev(node);
  HashNode next = lru_hash_node_get_hash_next(node);

  lru_hash_node_set_hash_next(prev, next);
  lru_hash_node_set_hash_prev(next, prev);

  // Devolvemos el lock y retornamos
  if (pthread_mutex_unlock(mutex) != 0)
    return -1;

  return 0;

}

// todo
int hashmap_delete_node(int key, HashMap map) { 

    // ! SETTEAR EL NODE A NULL ANTE DE HACER FREE

    return 0;

}

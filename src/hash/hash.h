#ifndef __HASH_H__
#define __HASH_H__

#include "nodes.h"

typedef LRUHashNode HashNode;

typedef HashNode Bucket;

typedef struct HashMap* HashMap;

typedef int (*HashFunction)(int);

int placeholder_hash(int n) { return n; }

/// @brief Crea un HashMap con funcion de hash \a hash y cantidad de buckets \a n_buckets.
HashMap hashmap_create(HashFunction hash, int n_buckets);

/// @brief Devuelve el bucket_number asociado a la key en el map.
int hashmap_find_bucket_number(int key, HashMap map);

/// @brief Devuelve el puntero al bucket asociado al bucket_number objetivo en el HashMap map, o NULL si falla.
Bucket hashmap_find_bucket(int bucket_number, HashMap map);

// @brief: Crea un nuevo nodo con el par clave-valor y lo inserta en el hashmap. No altera su prioridad,
// que es inicializada en null. Devuelve el puntero al nodo creado.
HashNode hashmap_insert(int key, int val, HashMap map);

// ! Ver que hacer si no existe en el mapa. Podriamos avisar por errno??
// @brief: Busca la clave en el HashMap objetivo, retornando su valor asociado, si es que existe.
LookupResult hashmap_lookup(int key, HashMap map);

/// @brief Busca la clave en el HashMap objetivo al igual que hashmap_lookup, pero devolviendo un puntero
//  al nodo en caso de existir, o NULL en caso contrario.
HashNode hashmap_lookup_node(int key, HashMap map);

// @brief: Elimina el nodo asociado a la clave objetivo del HashMap. Retorna 0 si lo elimina exitosamente,
// (ver si hacemos algo cuando el nodo no estaba en el hashmap, o si falla por otro motivo)
int hashmap_delete(int key, HashMap map);

// ? hashmap_update la definimos, o la hacemos directo con insert?

#endif // __HASH_H__

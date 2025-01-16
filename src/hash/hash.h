#ifndef __HASH_H__
#define __HASH_H__

#include "nodes.h"

typedef LRUHashNode HashNode;

typedef HashNode Bucket;

typedef struct HashMap* HashMap;

typedef int (*HashFunction)(int);

// @brief: Crea un nuevo nodo con el par clave-valor y lo inserta en el hashmap. No altera su prioridad,
// que es inicializada en null. Devuelve el puntero al nodo creado.
HashNode hashmap_insert(int key, int val, HashMap map);

// ! Ver que hacer si no existe en el mapa. Podriamos avisar por errno??
// @brief: Busca la clave en el HashMap objetivo, retornando su valor asociado, si es que existe.
int hashmap_lookup(int key, HashMap map);

// @brief: Elimina el nodo asociado a la clave objetivo del HashMap. Retorna 0 si lo elimina exitosamente,
// (ver si hacemos algo cuando el nodo no estaba en el hashmap, o si falla por otro motivo)
int hashmap_delete(int key, HashMap map);

// ? hashmap_update la definimos, o la hacemos directo con insert?

#endif // __HASH_H__

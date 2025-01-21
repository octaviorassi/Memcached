#ifndef __HASH_MAP_H__
#define __HASH_MAP_H__

#include "hashnode.h"

/** TODO:
 *  1. Ver que argumentos es necesario pasar al hashmap_create
 */

typedef struct HashMap* HashMap;

typedef int (*HashFunction)(int);

// int placeholder_hash(int n) { return n; }

/// @brief Crea un HashMap con funcion de hash \a hash y cantidad de buckets \a n_buckets.
HashMap hashmap_create(HashFunction hash, int n_buckets);

/// @brief Devuelve el bucket_number asociado a la key en el map.
int hashmap_get_bucket_number(int key, HashMap map);

/**
 * @brief Bloquea el mutex asociado a la clave en el mapa hash. 
 * @param key Clave objetivo.
 * @param map Puntero al mapa hash.
 * @return 0 si el bloqueo es exitoso (es decir, se posee el mutex), -1 en caso de error.
 */
int hashmap_get_key_lock(int key, HashMap map);


/**
 * @brief Libera el mutex asociado a la clave en el mapa hash. 
 * @param key Clave objetivo.
 * @param map Puntero al mapa hash.
 * @return 0 si la liberacion es exitosa, -1 en caso de error.
 */
int hashmap_release_key_lock(int key, HashMap map);

/// @brief Devuelve el puntero al bucket asociado al bucket_number objetivo en el HashMap map, o NULL si falla.

/**
 * @brief Obtiene el puntero al inicio del bucket asociado al numero de bucket objetivo en el HashMap, tomando ademas
 * posesion de su mutex correspondiente.
 * 
 * IMPORTANTE: La funcion obtiene el mutex asociado al bucket pero no lo libera, si no que este sigue bajo posesion de
 * la funcion que la invoca. Es necesario liberarlo cuando ya no es requerido.
 * 
 * @param bucket_number Numero de bucket buscado.
 * @param map HashMap objetivo.
 * @return Un puntero al inicio del bucket. Al retornar, si el puntero es no nulo, se tiene en posesion el mutex asociado
 * al nodo.
 */
HashNode hashmap_get_bucket(int bucket_number, HashMap map);

// @brief: Crea un nuevo nodo con el par clave-valor y lo inserta en el hashmap. No altera su prioridad,
// que es inicializada en null. Devuelve el puntero al nodo creado.

/**
 * @brief Inserta un par clave-valor al HashMap
 * 
 * IMPORTANTE: Esta funcion NO es thread-safe. Se debe obtener el lock asociado a la key previo a su invocacion, y
 * liberarlo tras finalizar su ejecucion.
 * 
 * IMPORTANTE: Asumimos que el par clave-valor no estaba presente anteriormente en la cache.
 * 
 * @param key Clave del par.
 * @param val Valor asociado a la clave.
 * @param map HashMap objetivo.
 * @return Retorna el puntero al nodo insertado en el HashMap, que es NULL si se produjo algun error.
 */
HashNode hashmap_insert(int key, int val, HashMap map);


/**
 * @brief Si la clave \a key pertenece a algun par en el HashMap,
 * actualiza su valor asociado \a val. Caso contrario, no hace nada.
 * 
 * IMPORTANTE: En caso de lograr actualizaro, ademas se obtiene el 
 * mutex asociado al nodo objetivo.Es responsabilidad del llamante
 * liberarlo posteriormente.
 * Esta funcion es thread-safe.
 * 
 * @param key Clave del par.
 * @param val Nuevo valor.
 * @param map HashMap objetivo.
 * @return Retorna el puntero al nodo actualizado en caso de exito, o NULL si no se
 * actualizo la clave objetivo.
 */
HashNode hashmap_update(int key, int val, HashMap map);

/**
 * @brief Busca el valor asociado a la clave en el HashMap.
 * 
 * IMPORTANTE: Esta funcion es thread-safe. No debe pedirse el mutex asociado a la clave antes de invocarla, ni
 * liberarlo tras terminada la ejecucion.
 * @param key Clave a buscar.
 * @param map HashMap objetivo.
 * @return Un \a LookupResult con el valor asociado y un status reflejando si la busqueda fue exitosa,
 * si no se encontro, o si se produjo algun error.
 */
LookupResult hashmap_lookup(int key, HashMap map);


/// @brief Busca la clave en el HashMap objetivo al igual que hashmap_lookup, pero devolviendo un puntero
//  al nodo en caso de existir, o NULL en caso contrario.

/**
 * @brief Busca el puntero al nodo asociado a la clave en el HashMap.
 * 
 * IMPORTANTE: En caso de encontrarlo, ademas se obtiene su mutex asociado, y
 * es responsabilidad del llamante liberarlo posteriormente.
 * Esta funcion es thread-safe.
 * 
 * @param key Clave buscada.
 * @param map HashMap objetivo.
 * @return Un puntero al nodo buscado, que es NULL en caso de
 * no encontrarlo.
 */
HashNode hashmap_lookup_node(int key, HashMap map);


// @brief: Elimina el nodo asociado a la clave objetivo del HashMap. Retorna 0 si lo elimina exitosamente,
// (ver si hacemos algo cuando el nodo no estaba en el hashmap, o si falla por otro motivo)
int hashmap_delete_node(int key, HashMap map);

/**
 *  @brief Libera la memoria asignada al HashMap objetivo.
 * 
 *  @param map El HashMap objetivo.
 */
void hashmap_destroy(HashMap map);

/**
 *  @brief Obtiene el mutex asociado al numero de bucket en el HashMap.
 * 
 *  @param bucket_number El numero del bucket.
 *  @param map El HashMap objetivo.
 * 
 *  @returns 0 en caso de exito, -1 si no.
 */
int hashmap_lock_zone_mutex(int bucket_number, HashMap map);

/**
 *  @brief Libera el mutex asociado al numero de bucket en el HashMap.
 * 
 *  @param bucket_number El numero del bucket.
 *  @param map El HashMap objetivo.
 * 
 *  @returns 0 en caso de exito, -1 si no.
 */
int hashmap_unlock_zone_mutex(int bucket_number, HashMap map);

#endif // __HASH_MAP_H__

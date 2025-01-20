#ifndef __HASH_NODE_H__
#define __HASH_NODE_H__

#include <stdio.h>
#include "../helpers/results.h"

/** TODO:
 *  Dudas sobre la estructura: 
 *  1.  Hacer chequeos o no en los gets. Por ejemplo, si el puntero es nulo, que devuelvo en get_key? Vale la pena?
 */

typedef struct HashNode* HashNode;

/**
 * @brief Crea un nuevo nodo.
 * @param key Clave del nodo.
 * @param val Valor asociado a la clave del nodo.
 * @return Un puntero al nodo creado, que es NULL si falla al asignarse memoria.
 */
HashNode hashnode_create(int key, int val);

/**
 * @brief Destruye el nodo objetivo, liberando la memoria asociada. Se asume que el nodo ya esta 'limpio'.
 * @param node El nodo a destruir.
 */
void hashnode_destroy(HashNode node);


/**
 * @brief Busca el valor asociado a la clave en el bucket iniciado en \a node.
 * 
 * @param key Clave a buscar.
 * @param node Bucket inicial.
 * @return Un \a LookupResult con el valor asociado y un status reflejando si la busqueda fue exitosa,
 * si no se encontro, o si se produjo algun error.
 */
LookupResult hashnode_lookup(int key, HashNode node);

/**
 * @brief Desconecta al nodo objetivo de sus vecinos, reconectando a sus nodos adyacentes.
 * 
 * IMPORTANTE: Esta funcion NO es thread-safe. Debe pedirse el mutex asociado a la clave antes de invocarla, y
 * liberarlo tras terminada la ejecucion.
 * 
 * @param node Nodo a limpiar.
 * @return 0 si es exitoso, 1 si se produjo un error.
 */
int hashnode_clean(HashNode node);


/**
 * @brief Obtiene la clave del nodo.
 * @param node Puntero al nodo.
 * @return La clave del nodo.
 */
int hashnode_get_key(HashNode node);

/**
 * @brief Establece la clave del nodo.
 * @param node Puntero al nodo.
 * @param key La nueva clave para el nodo.
 */
void hashnode_set_key(HashNode node, int key);

/**
 * @brief Obtiene el valor del nodo.
 * @param node Puntero al nodo.
 * @return El valor del nodo.
 */
int hashnode_get_val(HashNode node);

/**
 * @brief Establece el valor del nodo.
 * @param node Puntero al nodo.
 * @param val El nuevo valor para el nodo.
 */
void hashnode_set_val(HashNode node, int val);

/**
 * @brief Obtiene el nodo previo.
 * @param node Puntero al nodo.
 * @return Puntero al nodo previo.
 */
HashNode hashnode_get_prev(HashNode node);

/**
 * @brief Establece el nodo previo.
 * @param node Puntero al nodo.
 * @param prev Puntero al nuevo nodo previo.
 */
void hashnode_set_prev(HashNode node, HashNode prev);

/**
 * @brief Obtiene el nodo siguiente.
 * @param node Puntero al nodo.
 * @return Puntero al nodo siguiente.
 */
HashNode hashnode_get_next(HashNode node);

/**
 * @brief Establece el nodo siguiente.
 * @param node Puntero al nodo.
 * @param next Puntero al nuevo nodo siguiente.
 */
void hashnode_set_next(HashNode node, HashNode next);


#endif // __HASH_NODE_H__

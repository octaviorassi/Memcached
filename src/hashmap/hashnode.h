#ifndef __HASH_NODE_H__
#define __HASH_NODE_H__

#include <stdio.h>
#include "../src/cache/cache.h"
#include "../dynalloc/dynalloc.h"
#include "../helpers/results.h"

/** TODO:
 *  Dudas sobre la estructura: 
 *  1.  Hacer chequeos o no en los gets. Por ejemplo, si el puntero es nulo, que devuelvo en get_key? Vale la pena?
 */

// Forward-declaration de LRUNode para evitar incluirla
typedef struct LRUNode* LRUNode;

typedef struct HashNode* HashNode;


/**
 * @brief Crea un nuevo nodo.
 * 
 * @param key El puntero a la clave del nodo.
 * @param key_size El tamaño de la clave.
 * @param val El puntero al valor del nodo.
 * @param val_size El tamaño del valor.
 * @param cache Puntero a la cache donde se insertara el nodo.
 * 
 * @return Un puntero al nodo creado, que es NULL si falla al asignarse memoria.
 */
HashNode hashnode_create(void* key, size_t key_size, void* val, size_t val_size, Cache cache);


/**
 * @brief Destruye el nodo objetivo, liberando la memoria asociada. Se asume que el nodo ya esta 'limpio'.
 * 
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
 * @brief Busca el puntero al nodo asociado a la clave en el bucket iniciado en \a node.
 * 
 * @param key Clave a buscar.
 * @param node Bucket inicial.
 * @return Un puntero al nodo buscado, que es NULL en caso de no encontrarse.
 */
HashNode hashnode_lookup_node(int key, HashNode node);

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




// ! Obs: los setters para key y val van a tener que tomar a Cache como parametro para poder llamar a dynalloc.


/**
 * @brief Obtiene la clave del nodo.
 * 
 * @param node Puntero al nodo.
 * @return El puntero a la clave del nodo.
 */
void* hashnode_get_key(HashNode node);

/**
 * @brief Establece la clave del nodo.
 * 
 * @param node Puntero al nodo.
 * @param key El puntero a la nueva clave para el nodo.
 * @param new_key_size El tamaño de la nueva clave del nodo.
 * @param cache La cache a la que pertenece el nodo.
 * 
 * @return 0 si la operacion es exitosa, -1 si no.
 */
int hashnode_set_key(HashNode node, void* key, size_t new_key_size, Cache cache);


/**
 * @brief Obtiene el valor del nodo.
 * 
 * @param node Puntero al nodo.
 * @return El puntero al valor del nodo.
 */
void* hashnode_get_val(HashNode node);


/**
 * @brief Establece la clave del nodo.
 * 
 * @param node Puntero al nodo.
 * @param val El puntero al nuevo valor para el nodo.
 * @param new_val_size El tamaño del nuevo valor del nodo.
 * @param cache La cache a la que pertenece el nodo.
 * 
 * @return 0 si la operacion es exitosa, -1 si no.
 */
int hashnode_set_val(HashNode node, void* val, size_t new_val_size, Cache cache);

/**
 * @brief Obtiene el tamaño de la clave del nodo.
 * 
 * @param node Puntero al nodo.
 * @return El tamaño de la clave del nodo.
 */
size_t hashnode_get_key_size(HashNode node);


/**
 * @brief Establece el tamaño de la clave del nodo.
 * 
 * @param node Puntero al nodo.
 * @param key_size El tamaño de la clave del nodo.
 */
void hashnode_set_key_size(HashNode node, size_t key_size);


/**
 * @brief Obtiene el tamaño del valor del nodo.
 * 
 * @param node Puntero al nodo.
 * @return El tamaño del valor del nodo.
 */
size_t hashnode_get_val_size(HashNode node);


/**
 * @brief Establece el tamaño del valor del nodo.
 * 
 * @param node Puntero al nodo.
 * @param val_size El tamaño del valor del nodo.
 */
void hashnode_set_val_size(HashNode node, size_t val_size);


/**
 * @brief Obtiene el nodo previo.
 * @param node Puntero al nodo.
 * @return Puntero al nodo previo.
 */
HashNode hashnode_get_prev(HashNode node);

/**
 * @brief Establece el nodo previo.
 * 
 * @param node Puntero al nodo.
 * @param prev Puntero al nuevo nodo previo.
 */
void hashnode_set_prev(HashNode node, HashNode prev);

/**
 * @brief Obtiene el nodo siguiente.
 * 
 * @param node Puntero al nodo.
 * @return Puntero al nodo siguiente.
 */
HashNode hashnode_get_next(HashNode node);

/**
 * @brief Establece el nodo siguiente.
 * 
 * @param node Puntero al nodo.
 * @param next Puntero al nuevo nodo siguiente.
 */
void hashnode_set_next(HashNode node, HashNode next);

/**
 * @brief Obtiene el puntero al nodo de prioridad asociado.
 * 
 * @param node Puntero al nodo objetivo.
 * @return Puntero a su nodo de prioridad.
 */
LRUNode hashnode_get_prio(HashNode node);

/**
 * @brief Establece el puntero al nodo de prioridad.
 * @param node Puntero al nodo objetivo.
 * @param prio Puntero al nodo de prioridad.
 */
void hashnode_set_prio(HashNode node, LRUNode prio);

#endif // __HASH_NODE_H__

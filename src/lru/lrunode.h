#ifndef __LRU_NODE_H__
#define __LRU_NODE_H__

// Forward declaration
typedef struct Cache* Cache;

typedef struct LRUNode* LRUNode;
typedef struct HashNode* HashNode;


/**
 *  @brief Crea un nuevo LRUNode vacio.
 * 
 *  @param Cache La cache a la que pertenece la LRUQueue donde se insertara el nodo.
 *  @return Un puntero al LRUNode nuevo. 
 */
LRUNode lrunode_create(Cache cache);

/**
 *  @brief Devuelve un booleano representando si el nodo LRU esta 'limpio'. Diremos que esta limpio si no esta conectado a ningun nodo LRU.
 * 
 *  @param node El nodo LRU objetivo.
 *  @return 1 si esta limpio, 0 si no.
 */
int lru_node_is_clean(LRUNode node);


/**
 * @brief Destruye el nodo objetivo, liberando la memoria asociada. Se asume que el nodo ya esta 'limpio'.
 * @param node El nodo a destruir.
 */
void lrunode_destroy(LRUNode node);

/**
 * @brief Obtiene el nodo previo.
 * @param node Puntero al nodo LRU.
 * @return Puntero al nodo previo.
 */
LRUNode lrunode_get_prev(LRUNode node);

/**
 * @brief Establece el nodo previo.
 * @param node Puntero al nodo LRU.
 * @param prev Puntero al nuevo nodo previo.
 */
void lrunode_set_prev(LRUNode node, LRUNode prev);

/**
 * @brief Obtiene el nodo siguiente.
 * @param node Puntero al nodo LRU.
 * @return Puntero al nodo siguiente.
 */
LRUNode lrunode_get_next(LRUNode node);

/**
 * @brief Establece el nodo siguiente.
 * @param node Puntero al nodo LRU.
 * @param next Puntero al nuevo nodo siguiente.
 */
void lrunode_set_next(LRUNode node, LRUNode next);

/**
 * @brief Obtiene el nodo hash asociado.
 * @param node Puntero al nodo LRU.
 * @return Puntero al nodo hash.
 */
HashNode lrunode_get_hash_node(LRUNode node);

/**
 * @brief Establece el nodo hash asociado.
 * @param node Puntero al nodo LRU.
 * @param hash_node Puntero al nuevo nodo hash.
 */
void lrunode_set_hash_node(LRUNode node, HashNode hash_node);

/**
 * @brief Obtiene el numero de bucket del hashnode asociado a `node`.
 * @param node Puntero al nodo LRU.
 * @return El nmero de bucket del hash node asociado a `node`.
 * 
 */
unsigned int lrunode_get_bucket_number(LRUNode node);

/**
 * @brief Establece el numero de bucket del nodo objetivo.
 * @param node Puntero al nodo LRU.
 * @param bucket_number Numero de bucket del hash node asociado a `node`.
 */
void lrunode_set_bucket_number(LRUNode node, unsigned int bucket_number);

#endif // __LRU_NODE_H__
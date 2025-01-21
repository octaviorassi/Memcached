#ifndef __LRU_H__
#define __LRU_H__

#include "hashnode.h"

typedef struct LRUQueue* LRUQueue;

typedef struct LRUNode* LRUNode;

/**
 *  @brief Inicializa una LRU vacia.
 *  @return La LRUQueue generada.
 */
LRUQueue lru_queue_create();

/**
 * @brief Obtiene un puntero al nodo menos utilizado.
 * 
 * @param q La cola objetivo.
 * @return El nodo menos utilizado.
 */
LRUNode lru_queue_get_least_recent(LRUQueue q);

/**
 * @brief Obtiene el nodo mas recientemente utilizado de la cola.
 * 
 * @param q La cola
 * @return El nodo mas recientemente utilizado.
 */
LRUNode lru_queue_get_most_recent(LRUQueue q);

/// @brief: Inserta un nuevo nodo \a node como el mas reciente de la cola \a q.
/// @return: El puntero al nodo insertado, o NULL en caso de error.s
LRUNode lru_queue_add_recent(HashNode node, LRUQueue q);

/** 
 *  @brief: Desconecta un nodo de la LRUQueue (normalmente llamada previo a su eliminacion o relocacion),
 *  reconectando su nodo predecesor y sucesor de ser necesario.
 *  @return 0 si es exitoso, -1 si se produjo un error relacionado al mutex de \a q.
 **/
int lru_queue_node_clean(LRUNode node, LRUQueue q);

/// @brief: Desplaza el nodo objetivo al principio de la cola.
LRUNode lru_queue_set_most_recent(LRUNode node, LRUQueue q);

/* 
// @brief: Destruye la LRU liberando su memoria.
void lru_destroy(LRUQueue q);
*/

#endif // __LRU_H__
#ifndef __LRU_H__
#define __LRU_H__

#include "nodes.h"

// ! LRUNode no es mas que otro nombre para la estructura mas general, LRUHashNode
typedef LRUHashNode LRUNode;

typedef struct LRUQueue* LRUQueue;

/// @brief: Inicializa una LRU vacia.
LRUQueue lru_queue_create();

/// @brief: Obtiene el nodo menos recientemente usado
LRUNode lru_queue_get_least_recent(LRUQueue q);

/// @brief: Obtiene el nodo mas recientemente usado
LRUNode lru_queue_get_most_recent(LRUQueue q);

/// @brief: Inserta un nuevo nodo \a node como el mas reciente de la cola \a q.
/// @return: El puntero al nodo insertado, o NULL en caso de error.s
LRUNode lru_queue_add_recent(LRUNode node, LRUQueue q);

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
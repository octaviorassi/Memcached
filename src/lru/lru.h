#ifndef __LRU_H__
#define __LRU_H__

#include "nodes.h"

// ! LRUNode no es mas que otro nombre para la estructura mas general, LRUHashNode
typedef LRUHashNode LRUNode;

typedef struct LRUQueue* LRUQueue;

// @brief: Inicializa una LRU vacia.
LRUQueue lru_queue_init();

// @brief: Obtiene el ultimo elemento de la cola, i.e. el menos usado
LRUNode lru_queue_get_last(LRUQueue q);

// @brief: Inicializa una LRU vacia.
LRUNode lru_add_to_head(LRUNode node, LRUQueue q);

// @brief: Desconecta un nodo de la LRUQueue (normalmente llamada previo a su eliminacion),
// reconectando su nodo predecesor y sucesor de ser necesario.
LRUNode lru_node_clean(LRUNode node);

// @brief: Desplaza el nodo objetivo al principio de la cola.
LRUNode lru_move_to_head(LRUNode node, LRUQueue q)

/* 
// @brief: Destruye la LRU liberando su memoria.
void lru_destroy(LRUQueue q);
*/

#endif // __LRU_H__
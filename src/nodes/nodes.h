#ifndef __NODES_H__
#define __NODES_H__

// ! escondemos el struct en el .c y creamos getters en el .h para acceder
typedef struct LRUHashNode* LRUHashNode;

// @brief: Crea un nuevo LRUHashNode con el par clave-valor objetivo.
LRUHashNode lru_hash_node_create(int key, int val);

// @brief: Libera la memoria asociada al nodo objetivo.
int lru_hash_node_destroy(LRUHashNode node);



// ! getters

// @brief Obtiene la clave del nodo.
int lru_hash_node_get_key(const LRUHashNode node);

// @brief Obtiene el valor del nodo.
int lru_hash_node_get_value(const LRUHashNode node);

// @brief Obtiene el puntero al nodo previo en la lista LRU. Retorna NULL si no existe previo.
LRUHashNode lru_hash_node_get_lru_prev(const LRUHashNode node);

// @brief Obtiene el puntero al nodo siguiente en la lista LRU. Retorna NULL si no existe siguiente.
LRUHashNode lru_hash_node_get_lru_next(const LRUHashNode node);

// @brief Obtiene el puntero al nodo previo en la tabla hash. Retorna NULL si no existe previo.
LRUHashNode lru_hash_node_get_hash_prev(const LRUHashNode node);

// @brief Obtiene el puntero al nodo siguiente en la tabla hash. Retorna NULL si no existe siguiente.
LRUHashNode lru_hash_node_get_hash_next(const LRUHashNode node);



#endif // __NODES_H__
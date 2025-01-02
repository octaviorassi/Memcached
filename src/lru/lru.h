#ifndef __LRU_H__
#define __LRU_H__

typedef struct _LRUNode* LRUNode;

typedef struct _LRUQueue* LRUQueue;

/*  @brief: Crea un nuevo LRUNodo con los valores prev, next, y hashNode.
    @param[in]: prev es el nodo inmediatamente anterior en prioridad al nuevo nodo a crear
    @param[in]: next es el nodo inmediatamente siguiente en prioridad al nuevo nodo a crear
    @param[in]: es la informacion del hashNode asociada al nuevo LRUNode a crear */
LRUNode lru_node_create(LRUNode prev, LRUNode next, HashNode hashNode);

// @brief: Libera un LRUNode de manera no-recursiva.
void lru_node_destroy(LRUNode node);


// @brief: Inicializa una LRU vacia.
LRUQueue lru_queue_init();

// @brief: Inserta un nuevo nodo asociado al HashNode hashNode a la cola LRU
// @param[in] hashNode el nodo de la tabla hash asociado
void lru_queue_insert(LRUQueue q, HashNode hashNode);

// @brief: Eliminar al elemento con menos prioridad de la cola.
void lru_queue_free_space(LRUQueue q, int key);

// @brief Elimina el elemento asociado a la clave de la cola.
// @param[in] k La clave del elemento a eliminar
void lru_queue_delete(LRUQueue q, int k);

// @brief: Destruye la LRU liberando su memoria.
void lru_queue_destroy(LRUQueue q);

#endif // __LRU_H__
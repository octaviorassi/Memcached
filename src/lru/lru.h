#ifndef __LRU_H__
#define __LRU_H__

typedef struct _LRUNode* LRUNode;

typedef LRUNode LRUQueue;

// @brief: Inicializa una LRU vacia.
LRUQueue lru_init();

// @brief: Inicializa una LRU vacia.
LRUNode lru_insert(LRUQueue q, int key, int value);

// @brief: Eliminar al elemento con menos prioridad de la cola.
void lru_free_space(LRUQueue q, int key);

// @brief Elimina el elemento asociado a la clave de la cola.
// @params[in] k La clave del elemento a eliminar
void lru_delete(LRUQueue q, int k);

// @brief: Destruye la LRU liberando su memoria.
void lru_destroy(LRUQueue q);

#endif // __LRU_H__
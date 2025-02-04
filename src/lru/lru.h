#ifndef __LRU_H__
#define __LRU_H__

#include "../src/cache/cache.h"
#include "../dynalloc/dynalloc.h"

// Forward-declarations para no incluir sus header files
typedef struct HashNode* HashNode;
typedef struct LRUNode* LRUNode;

// Estructuras opacas
typedef struct LRUQueue* LRUQueue;

/**
 *  @brief Inicializa una LRU vacia.
 *  @return La LRUQueue generada.
 */
LRUQueue lru_queue_create();

/**
 *  @brief Establece al nodo objetivo como el mas reciente de la cola, asumiendo que puede ya haber formado parte de la cola. 
 * 
 *  @param node El nodo objetivo.
 *  @param q La cola LRU. 
 * 
 *  @return El puntero al nodo establecido.
 * 
 */
LRUNode lru_queue_set_most_recent(LRUNode node, LRUQueue q);

/**
 *  @brief Elimina el nodo menos recientemente utilizado de la cola, liberando su memoria asignada y retornando un puntero a su HashNode asociado.
 * 
 *  `IMPORTANTE` Se debe poseer el lock de la zona a la que este nodo pertenece o, en su defecto, el de todas las zonas.
 * 
 * @param q La cola LRU objetivo.
 * @return El puntero al HashNode asociado al nodo expulsado.
 * 
 */
HashNode lru_queue_evict(LRUQueue q);

/**
 *  @brief Destruye la cola LRU objetivo, liberando los recursos asignados sin liberar la memoria asociada a los nodos de la tabla hash. 
 * 
 * @param q La cola LRU a destruir.
 * @return 0 si es exitoso, -1 en caso de error.
 */
int lru_queue_destroy(LRUQueue q);

/**
 *  @brief Elimina el nodo objetivo de la cola LRU, liberando su memoria. No modifica la memoria asignada a su HashNode asociado.
 * 
 *  @param node El nodo a eliminar.
 *  @param q La cola LRU a la que pertenece.
 *
 *  @return 0 en caso de exito, -1 si se produjo un error. 
 */
int lru_queue_delete(LRUNode node, LRUQueue q);

// todo
void lru_queue_node_clean(LRUNode node, LRUQueue q);


#endif // __LRU_H__
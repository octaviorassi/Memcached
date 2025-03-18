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
 *  @brief Crea una LRU vacia.
 * 
 *  @return La LRUQueue generada.
 * 
 */
LRUQueue lru_queue_create();


/**
 *  @brief Lockea la LRUQueue objetivo.
 *  @param q La LRUQueue a lockear.
 *  
 *  @return 0 en caso exitoso, -1 si el puntero es NULL, un codigo de error en otro caso.
 */
int lru_queue_lock(LRUQueue q);


/**
 *  @brief Libera el lock de la LRUQueue objetivo.
 *  @param q La LRUQueue cuyo lock queremos liberar.
 *  
 *  @return 0 en caso exitoso, -1 si el puntero es NULL, un codigo de error en otro caso.
 */
int lru_queue_unlock(LRUQueue q);


/**
 *  @brief Establece al nodo objetivo como el mas reciente de la cola, asumiendo que puede ya haber formado parte de la cola. Esta funcion es `thread-safe`.
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
 *  @brief Obtiene el puntero al nodo menos recientemente utilizado de la LRUQueue objetivo.
 * 
 *  `IMPORTANTE` Debe contarse con el lock de la LRUQueue al invocarse a esta funcion.
 * 
 *  @param q La LRUQueue objetivo. 
 *  @return Un puntero al nodo menos utilizado, que es NULL en caso de que la cola este vacia.
 * 
 */
LRUNode lru_queue_get_least_recent(LRUQueue q);

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
 *  Esta funcion es thread-safe. No debe poseerse el lock de la LRUQueue al invocarla.
 * 
 *  @param node El nodo a eliminar.
 *  @param q La cola LRU a la que pertenece.
 *
 *  @return 0 en caso de exito, -1 si se produjo un error. 
 */
int lru_queue_delete_node(LRUNode node, LRUQueue q);

/**
 *  @brief Limpia un nodo de la LRUQueue. Es decir, lo desconecta de sus vecinos, si es que tiene.
 * 
 *  `IMPORTANTE` Esta funcion NO es thread-safe. Debe de lockearse la LRUQueue previo a ser invocada.
 * 
 *  @param node El nodo a limpiar. 
 *  @param q La cola LRU a la que pertenece.
 */
void lru_queue_node_clean(LRUNode node, LRUQueue q);

int lru_queue_get_count(LRUQueue q);

#endif // __LRU_H__
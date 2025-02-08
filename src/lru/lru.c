#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lru.h"
#include "lrunode.h"


struct LRUQueue {

  LRUNode most_recent;
  LRUNode least_recent;

  pthread_mutex_t* lock; 

};


LRUQueue lru_queue_create(Cache cache) { 
  
  pthread_mutex_t* lock = dynalloc(sizeof(pthread_mutex_t), cache);
  if (lock == NULL)
    return NULL;

  if (pthread_mutex_init(lock, NULL) != 0) {
    free(lock);
    return NULL;
  }

  LRUQueue queue = dynalloc(sizeof(struct LRUQueue), cache);
  if (queue == NULL) {
    free(lock);
    return NULL;
  }

  memset(queue, 0, sizeof(struct LRUQueue));
  queue->lock = lock;

  return queue;
  
}


inline int lru_queue_lock(LRUQueue q) {
  return (q == NULL) ? -1 : pthread_mutex_lock(q->lock);
}


inline int lru_queue_unlock(LRUQueue q) {
  return (q == NULL) ? -1 : pthread_mutex_unlock(q->lock);
}


static inline int lru_node_is_clean(LRUNode node) {
  return  lrunode_get_prev(node) == NULL &&
          lrunode_get_next(node) == NULL;
}


static LRUNode lru_queue_add_recent(LRUNode node, LRUQueue q) { 

  if (node == NULL) 
    return NULL;

  /** I.    El previo lru de node pasa a ser el mas reciente de q.
   *  II.   Si el mas reciente es no nulo, su siguiente pasa a ser node.
   *  III.  El mas reciente de la cola pasa a ser node.
   *  IV.   El siguiente de node es siempre NULL.
   */

  lrunode_set_prev(node, q->most_recent);
  lrunode_set_next(q->most_recent, node);

  q->most_recent = node;

  // Si es el primer nodo a insertar, tambien es el menos reciente.
  if (q->least_recent == NULL)
    q->least_recent = node; 

  lrunode_set_next(node, NULL);

  return node;

}


void lru_queue_node_clean(LRUNode node, LRUQueue q) { 
  
  LRUNode prev = lrunode_get_prev(node);
  LRUNode next = lrunode_get_next(node);

  lrunode_set_next(prev, next);
  lrunode_set_prev(next, prev);

  LRUNode lr = q->least_recent;
  LRUNode mr = q->most_recent;

  if (lr == node)
    q->least_recent = lrunode_get_next(lr);

  if (mr == node)
    q->most_recent  = lrunode_get_prev(mr);

}


LRUNode lru_queue_set_most_recent(LRUNode node, LRUQueue q) { 

  if (node == NULL)
    return NULL;

  lru_queue_lock(q);

  // Si ya formaba parte de la cola, lo desconectamos.
  if (!lru_node_is_clean(node))
    lru_queue_node_clean(node, q);

  // Y luego lo agregamos al inicio
  node = lru_queue_add_recent(node, q);

  lru_queue_unlock(q);
  
  return node;

}


HashNode lru_queue_evict(LRUQueue q) {

  // Lockeamos la cola y obtenemos el ultimo nodo.
  if (q == NULL || lru_queue_lock(q) < 0)
    return 0;

  int obtained_lock = 0;
  LRUNode lrunode = q->least_recent;
  HashNode hashnode;

  while (!obtained_lock) {

    HashNode hashnode = lrunode_get_hash_node(lrunode);

    // hashnode_get_key()
    // cache_trylock_key()

  }

  lru_queue_node_clean(lrunode, q);
  lrunode_destroy(lrunode);

  lru_queue_unlock(q);

  return hashnode;

}


LRUNode lru_queue_get_least_recent(LRUQueue q) {
  return (q == NULL) ? NULL : q->least_recent;
}


int lru_queue_destroy(LRUQueue q) {

  // Solo destruye la memoria que es propia de la LRU. No se mete con la Hash.
  if (q == NULL)
    return -1;

  lru_queue_lock(q);

  LRUNode tmp = q->least_recent;
  LRUNode next;

  while (tmp) {
    next = lrunode_get_next(tmp);
    lrunode_destroy(tmp);
    tmp = next;
  }

  lru_queue_unlock(q);

  pthread_mutex_destroy(q->lock);
  free(q);

  return 0;

}


int lru_queue_delete(LRUNode node, LRUQueue q) {

  if (node == NULL)
    return -1;

  lru_queue_lock(q);

  lru_queue_node_clean(node, q);
  lrunode_destroy(node);

  lru_queue_unlock(q);

  return 0;
  
}





/** -----------------------------------
 *  Funciones auxiliares no exportadas.
 *  -----------------------------------
 */






#include "lru.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


struct LRUQueue {

  LRUNode most_recent;
  LRUNode least_recent;

  pthread_mutex_t* lock; // ? mejor el mutex o el puntero al mutex? porq las funciones usan el puntero.

};

LRUQueue lru_queue_init() { 
  
  pthread_mutex_t* lock = malloc(sizeof(pthread_mutex_t));
  if (lock == NULL)
    return NULL;

  if (pthread_mutex_init(lock, NULL) != 0) {
    free(lock);
    return NULL;
  }

  LRUQueue queue = malloc(sizeof(struct LRUQueue));
  if (queue == NULL) {
    free(lock);
    return NULL;
  }

  memset(queue, 0, sizeof(struct LRUQueue));
  queue->lock = lock;

  return queue;
  
}

// ? si chequeo null y luego pido lock, es posible que vea que no es null, otro proceso elimine la cola,
// ? y cuando vaya a pedir el lock, el puntero ya sea null.
// ? pero, si pido el lock y luego chequeo si es null, puede que este desreferenciando a un puntero null.
inline int lru_queue_lock(LRUQueue q) {
  return (q == NULL) ? -1 : pthread_mutex_lock(q->lock);
}

inline int lru_queue_unlock(LRUQueue q) {
  return (q == NULL) ? -1 : pthread_mutex_unlock(q->lock);
}


// ! Quizas esta deberia ser la unica funcion en vez de tener tanto add_recent como set_most_recent
// ! Son practicamente iguales, nada mas que add_recent asume que es un nuevo nodo mientras que set asume
// ! que ya era parte de la cola, por lo que antes de insertarlo lo limpia.
LRUNode lru_queue_add_recent(LRUNode node, LRUQueue q) { 

  if (node == NULL) 
    return NULL;

  /** I.    El previo lru de node pasa a ser el mas reciente de q.
   *  II.   Si el mas reciente no nulo, su siguiente pasa a ser node.
   *  III.  El mas reciente de la cola pasa a ser node.
   *  IV.   El siguiente de node es siempre NULL.
   */

  if (lru_queue_lock(q) < 0)
    return NULL;

  lru_hash_node_set_lru_prev(node, q->most_recent);

  lru_hash_node_set_lru_next(q->most_recent, node);

  q->most_recent = node; 

  lru_hash_node_set_lru_next(node, NULL);

  if (lru_queue_unlock(q) < 0)
    return NULL;

  return node;

}

int lru_queue_node_clean(LRUNode node, LRUQueue q) { 
  
  if (lru_queue_lock(q) < 0) return -1;

  LRUNode prev = lru_hash_node_get_lru_prev(node);
  LRUNode next = lru_hash_node_get_lru_next(node);

  lru_hash_node_set_lru_next(prev, next);
  lru_hash_node_set_lru_prev(next, prev);

  if (lru_queue_unlock(q) < 0) return -1;

  return 0;

}

// ! Ver cuales funciones deberian tomar lock y cuales no.
// ! Por ejemplo, aca quizas seria mejor que solo se tome el lock una vez y se haga la limpieza
// ! y la insercion juntas, pero como las funciones clean y add_recent ambas lo toman, el lock es
// ! tomado, devuelto, y de nuevo tomado.
LRUNode lru_queue_set_most_recent(LRUNode node, LRUQueue q) { 

  // Primero lo desconectamos (clean)
  if (lru_queue_node_clean(node, q) < 0)
    return NULL;

  // Y luego lo agregamos al inicio
  return lru_queue_add_recent(node, q);
  
}

inline LRUNode lru_queue_get_least_recent(LRUQueue q) { 
  return (q == NULL) ? NULL : q->least_recent;
}

inline LRUNode lru_queue_get_most_recent(LRUQueue q) { 
  return (q == NULL) ? NULL : q->most_recent;
}


#include "lru.h"
#include "../list/list.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

struct _LRUNode {

  struct _LRUNode* prev;
  struct _LRUNode* next;

  HashNode hashNode;
};


struct _LRUQueue {

  pthread_mutex_t lock;

  LRUNode first;  //  Recently used
  LRUNode last;   //  Least used

};

LRUNode lru_node_create(LRUNode prev, LRUNode next, HashNode hashNode) {

  LRUNode lru_node = malloc(sizeof(struct _LRUNode));

  if (lru_node == NULL) {
    // ! Mejorar el manejo de errores?
    return NULL;
  }

  lru_node->next = next;
  lru_node->prev = prev;
  lru_node->hashNode = hashNode;

  return lru_node;
  
}

void lru_node_destroy(LRUNode node) {

  if (node == NULL) {
    return;
  }

  hash_node_destroy(node->hashNode);
  free(node);
}


LRUQueue lru_queue_init() { 

  LRUQueue lru_queue = malloc(sizeof(struct _LRUQueue));

  if (lru_queue == NULL) {
    return NULL;
  }

  if (pthread_mutex_init(&lru_queue->lock, NULL) != 0) {
    /*  Si no puede inicializarse el mutex todo el init falla,
        liberamos la memoria y devolvemos null */
    free(lru_queue); 
    return NULL;
  }

};

void lru_queue_insert(LRUQueue q, HashNode hashNode) {

  if (q == NULL) {
    return;
  }

  pthread_mutex_lock(&q->lock);

  LRUNode second = q->first;

  LRUNode new_lru_node = lru_node_create(second, NULL, hashNode);

  // Si el nodo que antes era primero es no nulo, entonces su siguiente es el nuevo nodo
  if (second != NULL) {
    second->next = new_lru_node;
  }

  q->first = new_lru_node;

  // Si la cola estaba vacia, entonces new_lru_node es tanto first como last
  if (q->last == NULL) {
    q->last = new_lru_node;
  }

  pthread_mutex_unlock(&q->lock);

}

// ! Cuantos eliminar? Si me pasan 0 como argumento elimino el ultimo, si es mayor a 0 busco el que me pidieron?
void lru_queue_free_space(LRUQueue q, int key);

void lru_queue_delete(LRUQueue q, int k);


// ! Es mejor si la LRUQueue destruye la BucketList o la BucketList destruye a la LRU?
// ! Obs. que asi como en lru_node_destroy llamo a hash_node_destroy, podria llamar a lru_node_destroy desde hash_node_destroy
void lru_queue_destroy(LRUQueue q) {

  if (q == NULL) {
    return;
  }

  // ! Seria buena idea tomar el lock mientras elimino los nodos, y por ultimo devolverlo y liberarlo?
  LRUNode tmp = q->first;
  LRUNode next;

  while (tmp) {
    next = tmp->next;
    lru_node_destroy(tmp);
    tmp = next;
  }

  pthread_mutex_destroy(&q->lock);

  free(q);

};


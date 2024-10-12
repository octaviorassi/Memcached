#include "hash.h"
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <pthread.h>


#define NODE_SIZE 100
#define INITIAL_CAPACITY 100


typedef struct _Node {

  char* key;
  char* value;
  struct _Node *next;
  
} Node;

typedef Node* List;


void node_init(Node* node, char* key, char* value) {

  node->key = malloc(sizeof(char) * (strlen(key) + 1));
  strcpy(node->key, key);

  node->value = malloc(sizeof(char) * (strlen(value) + 1));
  strcpy(node->value, value);

  node->next = NULL;
}


struct _HashMap {

	int n_elements; // Cantidad de elementos
  int capacity;   // Capacidad de la hash

	List* arr; // Arreglo de listas enlazadas

  pthread_mutex_t mutex; // Mutex para modificar el hash
};


HashMap* hash_create() { return malloc(sizeof(struct _HashMap)); }


void hash_init(HashMap* hash) {

	hash->capacity = INITIAL_CAPACITY;
	hash->n_elements = 0;

	hash->arr = malloc(sizeof(List) * INITIAL_CAPACITY);

  pthread_mutex_init(&hash->mutex, NULL);

  for (int i = 0 ; i < INITIAL_CAPACITY ; i++) 
    hash->arr[i] = NULL;	
}


static unsigned long hash_function(char* key) {
	
  unsigned long hashval;
  unsigned long i;

  for (i = 0, hashval = 0 ; i < strlen(key) ; ++i, key++)
    hashval = *key + 31 * hashval;
  
  return hashval;
}


void hash_insert(HashMap* hash, char* key, char* value) {

  pthread_mutex_lock(&hash->mutex);

	int idx = hash_function(key) % hash->capacity;

  // La lista esta vacia, colocamos el key-value
	if (hash->arr[idx] == NULL) {
    
    Node* new_node = malloc(sizeof(struct _Node));
    node_init(new_node, key, value);

    hash->arr[idx] = new_node;
    pthread_mutex_unlock(&hash->mutex);
    return;
	}

	// La lista enlazada no esta vacia
	else {

    // La busco en la lista a ver si quiero actualizarla o crear nuevo nodo
    Node* tmp = hash->arr[idx];
    int found = 0;

    while (!found && tmp) {
      found = strcmp(tmp->key, key) == 0;
      if (!found) tmp = tmp->next;
    }

    // si ya existia, actualiza el valor
    if (found) {
        free(tmp->value); // Libero el anterior
        tmp->value = malloc(sizeof(char) * (strlen(value) + 1)); // Doy nueva memoria
        strcpy(tmp->value, value); // Copiamos el valor
    }

    // Si no existia, la agrego como inicio de la lista enlazada
    else {
        
      Node* new_node = malloc(sizeof(struct _Node));
      node_init(new_node, key, value);

      new_node->next = hash->arr[idx];
      hash->arr[idx] = new_node;
    }
  }

  pthread_mutex_unlock(&hash->mutex);
	return;
}


void hash_delete(HashMap* hash, char* key) {

  pthread_mutex_lock(&hash->mutex);

	int idx = hash_function(key) % hash->capacity;

	Node* prev_node = NULL;

  Node* curr_node = hash->arr[idx];

	while (curr_node != NULL) {

		if (strcmp(key, curr_node->key) == 0) {

			if (curr_node == hash->arr[idx]) 
				hash->arr[idx] = curr_node->next;

			else {
				prev_node->next = curr_node->next;
			}

      free(curr_node->key);
      free(curr_node->value);
			free(curr_node);
			break;
		}

		prev_node = curr_node;
		curr_node = curr_node->next;
	}

  pthread_mutex_unlock(&hash->mutex);
	return;
}

char* hash_search(HashMap* hash, char* key) {

  pthread_mutex_lock(&hash->mutex);

	int idx = hash_function(key) % hash->capacity;

  char* result;

	Node* head = hash->arr[idx];
	while (head != NULL) {

		if (strcmp(head->key, key) == 0) {
      result = malloc(sizeof(char) * (strlen(head->value) + 1)); // Pido memoria
      strcpy(result, head->value); // Copio
      pthread_mutex_unlock(&hash->mutex); 
			return result; // Devuelvo !! Despues hay que liberar la memoria
		}

		head = head->next;
	}

  // Si no esta, devolvemos NULL
  pthread_mutex_unlock(&hash->mutex);
  return NULL;
}
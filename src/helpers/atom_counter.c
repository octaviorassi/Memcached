#include <stdlib.h>
#include <pthread.h>
#include "atom_counter.h"

struct AtomCounter {

    Counter counter;

    pthread_mutex_t lock;
    
};


AtomCounter atom_counter_create(unsigned int initial_value) {

    AtomCounter atom_counter = malloc(sizeof(struct AtomCounter));

    if (atom_counter == NULL)
        return NULL;

    atom_counter->counter = initial_value;
    
    if (pthread_mutex_init(&atom_counter->lock, NULL) != 0) {
        free(atom_counter);
        return NULL;
    }

    return atom_counter;

}


Counter atom_counter_get(AtomCounter counter) {

    if (counter == NULL)
        return 0;
    
    pthread_mutex_lock(&counter->lock);

    Counter count = counter->counter;

    pthread_mutex_unlock(&counter->lock);

    return count;
    
}


int atom_counter_inc(AtomCounter counter) {

    if (counter == NULL)
        return -1;
    
    pthread_mutex_lock(&counter->lock);

    counter->counter++;

    pthread_mutex_unlock(&counter->lock);

    return 0;

}


int atom_counter_dec(AtomCounter counter) {

    if (counter == NULL)
        return -1;
    
    pthread_mutex_lock(&counter->lock);

    if (counter->counter > 0)
        counter->counter--;

    pthread_mutex_unlock(&counter->lock);

    return 0;

}


int atom_counter_add(AtomCounter counter, Counter n) {

    if (counter == NULL)
        return -1;

    pthread_mutex_lock(&counter->lock);

    counter->counter += n;

    pthread_mutex_unlock(&counter->lock);

    return 0;

}


int atom_counter_drop(AtomCounter counter, Counter n) {

    if (counter == NULL)
        return -1;

    pthread_mutex_lock(&counter->lock);
    
    if (counter->counter > n)
        counter->counter -= n;
    else
        counter->counter = 0;

    pthread_mutex_unlock(&counter->lock);

    return 0;

}


int atom_counter_destroy(AtomCounter counter) {

    if (counter == NULL)
        return -1;

    pthread_mutex_destroy(&counter->lock);    

    free(counter);

    return 0;

}

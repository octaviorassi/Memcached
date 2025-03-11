#ifndef __ATOM_COUNTER_H__
#define __ATOM_COUNTER_H__

#define COUNTER_FORMAT "%lu"
typedef __uint64_t Counter;

typedef struct AtomCounter* AtomCounter;


AtomCounter atom_counter_create(unsigned int initial_value);

Counter atom_counter_get(AtomCounter counter);

int atom_counter_inc(AtomCounter counter);

int atom_counter_dec(AtomCounter counter);

int atom_counter_add(AtomCounter counter, Counter n);

int atom_counter_drop(AtomCounter counter, Counter n);

int atom_counter_destroy(AtomCounter counter);

#endif // __ATOMIC_COUNTER_H__
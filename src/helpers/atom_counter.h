#ifndef __ATOM_COUNTER__
#define __ATOM_COUNTER__

typedef struct AtomCounter* AtomCounter;

AtomCounter atom_counter_create(unsigned int initial_value);

unsigned int atom_counter_get(AtomCounter counter);

int atom_counter_inc(AtomCounter counter);

int atom_counter_dec(AtomCounter counter);

int atom_counter_destroy(AtomCounter counter);

#endif // __ATOMIC_COUNTER__
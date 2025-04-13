#ifndef __HASH_H__
#define __HASH_H__

#include <stdlib.h>

/* Funcion de hash de K&R */

unsigned long kr_hash(char* key, size_t size);

unsigned long dek_hash(char *key, size_t size);

#endif // __HASH_H__
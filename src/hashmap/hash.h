#ifndef __HASH_H__
#define __HASH_H__

#include <stdlib.h>

/* Funcion de hash de K&R */

unsigned long kr_hash(char* key, size_t size) {
	
  unsigned long hashval;
  unsigned long i;

  for (i = 0, hashval = 0 ; i < size ; ++i, key++)
    hashval = *key + 31 * hashval;
  
  return hashval;
}


#endif // __HASH_H__
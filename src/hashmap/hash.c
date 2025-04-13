#include "hash.h"

unsigned long kr_hash(char* key, size_t size) {
	
  unsigned long hashval;
  unsigned long i;

  for (i = 0, hashval = 0 ; i < size ; ++i, key++)
    hashval = *key + 31 * hashval;
  
  return hashval;
}

unsigned long dek_hash(char *key, size_t size)
{
  unsigned long hash = size;
  while (*key)
  {
    hash <<= 5;
    hash ^= (hash >> 27);
    hash ^= *key;
    key++;
  }
  return hash;
}
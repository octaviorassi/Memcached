#ifndef __CACHE_H__
#define __CACHE_H__

#include "../helpers/results.h"

typedef struct Cache* Cache;

/// todo: agregar que se pueda pasar los parametros por argumento.
/// @brief Inicializa una cache con parametros por defecto.
Cache cache_create();

/// @brief Devuelve el LookupResult resultante de buscar la clave asociada a la key en la cache.
/// @return Un LookupResult con la clave asociada en caso de exito, o un status de error en caso contrario. 
LookupResult cache_get(Cache cache, int key);

/// @brief Inserta el par clave-valor en la cache dada. De no contarse con espacio suficiente, 
/// aplica la politica de desalojo para generarlo.
int cache_put(Cache cache, int key, int value);


/// @brief Elimina el par clave-valor asociado a \a key en la cache objetivo. 
/// @return -1 si se produjo un error, 0 si es exitoso, 1 si el par no estaba en la cache.
int cache_delete(Cache cache, int key);

/// @brief //todo imprime en pantalla las stats? las envia como string?
void cache_stats(Cache cache);

/// @brief Libera la memoria de todos los componentes de la cache.
void cache_destroy(Cache cache);

#endif // __CACHE_H__
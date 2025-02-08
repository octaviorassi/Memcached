#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "cache/cache.h"

#define KEY_SIZE 5
#define KEY_COUNT 5
#define VAL_SIZE 5
#define VAL_COUNT 5

int main() {

    setbuf(stdout, NULL);

    // Creamos la cache
    Cache cache = cache_create(kr_hash, 10);

    // Definimos los pares key-value
    char* keys[KEY_COUNT];
    int*  vals[VAL_COUNT];
    

    const char* names[] = {
        "octa",
        "juli",
        "tito",
        "busa",
        "axel"
    };


    int nums[] = { 1, 2, 3, 4, 5 };

    for (int i = 0; i < KEY_COUNT; i++) {

        keys[i] = malloc(KEY_SIZE);
        vals[i] = malloc(sizeof(int));

        strncpy(keys[i], names[i], KEY_SIZE);
        keys[i][KEY_SIZE - 1] = '\0';

        *vals[i] = nums[i];

        PRINT("inserted key-value pair: %s / %i", keys[i], *vals[i]);

    }

    // Insertamos
    for (int i = 0; i < KEY_COUNT; i++)
        PRINT("iteration %i | cache_put return: %i", i, cache_put(keys[i], KEY_SIZE, vals[i], VAL_SIZE, cache));

    // Recuperamos las claves
    for (int i = 0; i < KEY_COUNT; i++) {

        LookupResult lr = cache_get(keys[i], KEY_SIZE, cache);

        if (lookup_result_is_ok(lr))
            PRINT("GET: (%s, %i)", keys[i], *((int*) lookup_result_get_value(lr)));
        else
            PRINT("Failed to GET the key %s", keys[i]);
    }

    // Eliminemos las que estan en posiciones pares
    PRINT("Comenzando la eliminacion.");
    for (int i = 0; i < KEY_COUNT; i++)
        if (i % 2 == 0)
            cache_delete(keys[i], KEY_SIZE, cache);

    // Recuperamos las claves nuevamente.
    for (int i = 0; i < KEY_COUNT; i++) {

        LookupResult lr = cache_get(keys[i], KEY_SIZE, cache);

        if (lookup_result_is_ok(lr))
            PRINT("GET: (%s, %i)", keys[i], *((int*) lookup_result_get_value(lr)));
        else
            PRINT("Failed to GET the key at address %p", keys[i]);
    }

    // Mostramos estadisticas.
    cache_stats(cache);

    return 0;
}


int main2() {

    setbuf(stdout, NULL);

    // Creamos la cache
    Cache cache = cache_create(kr_hash, 10);

    char name[] = "octavio";
    char* key = malloc(sizeof(name));
    strcpy(key, name);

    int* val = malloc(sizeof(int));
    *val = 10;

    // Insertamos
    cache_put(key, sizeof(key), val, sizeof(val), cache);

    // Getteamos
    LookupResult lr = cache_get(key, KEY_SIZE, cache);

    if (lookup_result_is_ok(lr))
        PRINT("GET exitoso: key %s | val %i", key, *val);
    else
        PRINT("GET fallo.");


    return 0;

}
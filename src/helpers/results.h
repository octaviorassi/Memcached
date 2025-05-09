#ifndef __RESULTS_H__
#define __RESULTS_H__

typedef enum {
    OK,     
    ERROR,
    MISS
} Status;

typedef struct LookupResult {

    void*   ptr;
    size_t size;
    
    Status  status;
    
} LookupResult;

/// @brief Devuelve un LookupResult con el valor objetivo y status OK.
LookupResult create_ok_lookup_result(void* ptr, size_t size);

/// @brief Devuelve un LookupResult con valor 0 y status ERROR.
LookupResult create_error_lookup_result();

/// @brief Devuelve un LookupResult con valor 0 y status MISS.
LookupResult create_miss_lookup_result();

/// @brief Devuelve 1 si el status del LookupResult objetivo es ERROR, o 0 en caso contrario.
int lookup_result_is_error(LookupResult lr);

/// @brief Devuelve 1 si el status del LookupResult objetivo es OK, o 0 en caso contrario.
int lookup_result_is_ok(LookupResult lr);

/// @brief Devuelve 1 si el status del LookupResult objetivo es MISS, o 0 en caso contrario.
int lookup_result_is_miss(LookupResult lr);

/// @brief Devuelve el valor almacenado en el LookupResult.
void* lookup_result_get_value(LookupResult lr);

/// @brief Devuelve el tamaño del valor almacenado en el LookupResult.
size_t lookup_result_get_size(LookupResult lr);

#endif // __RESULTS_H__

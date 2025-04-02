#include <stdlib.h>
#include "results.h"

static inline LookupResult create_lookup_result(void* ptr, size_t size, Status status) {
    return (LookupResult){ptr, size, status};
}

inline LookupResult create_ok_lookup_result(void* ptr,size_t size) {
    return create_lookup_result(ptr, size, OK);
}

inline LookupResult create_error_lookup_result() {
    return create_lookup_result(0, 0, ERROR);
}

inline LookupResult create_miss_lookup_result() {
    return create_lookup_result(0, 0, MISS);
}

inline int lookup_result_is_error(LookupResult lr) {
    return lr.status == ERROR;
}

inline int lookup_result_is_ok(LookupResult lr) {
    return lr.status == OK;
}

inline int lookup_result_is_miss(LookupResult lr) {
    return lr.status == MISS;
}

void* lookup_result_get_value(LookupResult lr) {
    return lr.ptr;
}

size_t lookup_result_get_size(LookupResult lr) {
    return lr.size;    
}


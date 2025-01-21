#include <stdlib.h>
#include "results.h"

static inline LookupResult create_lookup_result(int value, Status status) {
    return (LookupResult){value, status};
}

inline LookupResult create_ok_lookup_result(int value) {
    return create_lookup_result(value, OK);
}

inline LookupResult create_error_lookup_result() {
    return create_lookup_result(0, ERROR);
}

inline LookupResult create_miss_lookup_result() {
    return create_lookup_result(0, MISS);
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

int lookup_result_get_value(LookupResult lr) {
    return lr.status;
}

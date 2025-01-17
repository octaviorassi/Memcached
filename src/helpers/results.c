#include <stdlib.h>
#include "results.h"

inline LookupResult create_lookup_result(int value, Status status) {
    return (LookupResult){value, status};
}

inline LookupResult create_ok_lookup_result(int value) {
    return create_lookup_result(value, OK);
}

LookupResult create_error_lookup_result() {
    return create_lookup_result(0, ERROR);
}

inline int lookup_result_is_error(LookupResult lr) {
    return lr.status == ERROR;
}

inline int lookup_result_is_ok(LookupResult lr) {
    return lr.status == OK;
}

int lookup_result_get_value(LookupResult lr) {
    return lr.status;
}

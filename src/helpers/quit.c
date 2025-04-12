#include <stdlib.h>
#include <stdio.h>
#include "quit.h"

void quit(char* error) {
  perror(error);
  abort();
}
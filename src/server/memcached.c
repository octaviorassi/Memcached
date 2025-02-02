#include "memcached.h"
#include <stdio.h>
#include <unistd.h>


int main(int argc, char** argv) {

  // Sabemos que está siendo ejecutado con sudo (si es que queria bindearse a puerto privileagiado)

  // Creamos socket TCP Stream
  
  // Lo bindemos al puerto que nos pasaron
  
  // Cambiamos nuestros permisos - Capabilities

  // Seteamos el limite de nuetra memoria

  // Hacemos un execv sobre el server, pasandole el socket si es necesario  

  return 0;
}
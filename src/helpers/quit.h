#ifndef __QUIT_H__
#define __QUIT_H__

/**
 *  @brief Imprime el mensaje `error` explicando el valor de `errno` y aborta la ejecucion generando un core dump al invocar a `abort()`.
 * 
 *  @param error Mensaje de error explicando el valor de error de errno.
 */
void quit(char* error);

#endif // __QUIT_H__
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/ip.h>
#include <sys/epoll.h>
#include <netinet/in.h>
#include <wait.h>
#include <fcntl.h>
#include <errno.h>
#include <pthread.h>

typedef enum {
  PUT       = 11,
  DEL       = 12,
  GET       = 13,
  STATS     = 21,
  OK        = 101,
  EINVALID  = 111,
  ENOTFOUND = 112,
  EBINARY   = 113,
  EBIG      = 114,
  EUNK      = 115
} Command;

int option = 0;

int largoDefault = 9;
char value[9] = {131,104,3,97,1,97,2,97,3};

void readn(int fd, void *buf, int len) {

  int i = 0;
  int rc;

  while (i < len) {

    rc = read(fd, buf + i, len-i);
    if (rc <= 0)
      exit(1);
    i += rc;
  }
}

void recv_var(int fd, int* lenp, void** bufp) {

  void* buf;
  int len;
  char lbuf[4];

  readn(fd, lbuf, 4);

 int len_net = *(int*)lbuf;

 len = ntohl(len_net);

 buf = malloc(len);
 if (!buf) exit(1);

 readn(fd,buf,len);

 *bufp = buf;
 *lenp = len;
}

int main(int argc, char** argv) {

  int server_socket = atoi(argv[1]);

  int client_socket = accept(server_socket, NULL, NULL);
  printf("[] Nuevo cliente aceptado.\n");
  
  while(1) {

    char comand;
    printf("Resultado recv: %ld\n", recv(client_socket, &comand, 1, 0));

    printf("[] Command: %d\n", comand);

    if (comand != STATS) {
      
      int keyLen;
      void* key;
      recv_var(client_socket, &keyLen, &key);

      printf("[] Largo clave: %d\n", keyLen);
      printf("[] Clave: ");
      printf("[");
      for (int i = 0; i < keyLen ; i++) { printf("%c ", ((char*)key)[i]); }
      printf("]"); printf("\n");
    }

    if (comand == PUT) {
    int valueLen;
    void* value;
    recv_var(client_socket, &valueLen, &value);
    printf("[] Largo valor: %d\n", valueLen);

    printf("[] Valor: ");
    printf("[");
    for (int i = 0; i < valueLen ; i++) { printf("%c ", ((char*)value)[i]); }
    printf("]"); printf("\n");
    
    }

    char* response;
    int largo;

    if (comand == PUT) {
      
      response = malloc(1);
      largo = 1;
      if (option) {
        response[0] = OK;
      }

      else {
        response[0] = EBIG;
      }

    }

    if (comand == GET) {
      
      if(option) {

        largo = 1;
        response = malloc(1);
        response[0] = ENOTFOUND;
      }

      else {
        largo = largoDefault + 1 + 4;
        response = malloc(largo);
        response[0] = OK;
        int dadoVuelta = ntohl(largoDefault);
        memcpy(response + 1, &dadoVuelta, 4);
        memcpy(response + 5, value, largoDefault);
      }
    }

    if (comand == DEL) {

      response = malloc(1);
      largo = 1;
      if (option) {
        response[0] = OK;
      }

      else {
        response[0] = ENOTFOUND;
      }

    }

    if (comand == STATS) {
      // Implementar
    }


    send(client_socket, response, largo, 0);

    free(response);
    option = 1 - option;

  }
  close(server_socket);
  close(client_socket);
  
  return 0;
}



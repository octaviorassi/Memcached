#ifndef __CACHE_SERVER_UTILS_H__
#define __CACHE_SERVER_UTILS_H__

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

#include "../dynalloc/dynalloc.h"
#include "../cache/cache.h"

#define LENGTH 4


typedef enum {

  PARSING_COMMAND,
  PARSING_KEY_LEN,
  PARSING_KEY,
  PARSING_VALUE_LEN,
  PARSING_VALUE,
  PARSING_FINISHED

} ParsingStage;


typedef struct {

  int socket;

  char command;
  
  char key_size_buffer[LENGTH]; 
  int key_size;
  char* key;

  char value_size_buffer[LENGTH];
  int value_size;
  char* value;

  int parsing_index;
  ParsingStage parsing_stage;

} Data;

typedef enum {

  PUT       = 11,
  DEL       = 12,
  GET       = 13,
  STATS     = 21,
  OKAY      = 101,
  EINVALID  = 111,
  ENOTFOUND = 112,
  EBINARY   = 113,
  EBIG      = 114,
  EUNK      = 115

} Command;

typedef struct {

  int server_epoll;
  int server_socket;
  int thread_number;
  // Hash

} ThreadArgs;

typedef struct {

  int server_epoll;
  int server_socket;
  int num_threads;
  Cache cache;

} ServerArgs;

Data* create_new_client_data(int socket);

void quit(char* );

void recv_socket(int socket, char* message_buffer, int size, Data* data);

int send_socket(int socket, char* message, int size);

void parse_request(Data* data);

void handle_request(Data* data);

void handle_request(Data* data);

void reset_client_data(Data* data);

void reconstruct_client_epoll(int fd, struct epoll_event* ev, Data* data);

Data* create_new_client_data(int client_socket);


#endif // __CACHE_SERVER_UTILS_H__
#ifndef __CACHE_SERVER_MODELS_H__
#define __CACHE_SERVER_MODELS_H__

#define LENGTH 4

typedef enum {

  PARSING_COMMAND,
  PARSING_KEY_LEN,
  PARSING_KEY,
  PARSING_VALUE_LEN,
  PARSING_VALUE,
  PARSING_FINISHED

} ParsingStage;

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

} ThreadArgs;

typedef struct {

  int server_epoll;
  int server_socket;
  int num_threads;
  Cache cache;

} ServerArgs;

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

} ClientData;



#endif // __CACHE_SERVER_MODELS_H__
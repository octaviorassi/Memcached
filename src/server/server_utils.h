#ifndef __SERVER_UTILS_H__
#define __SERVER_UTILS_H__

#define LENGTH 4

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
  OK        = 101,
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
  // Hash

} ServerArgs;

typedef enum {

  PARSING_COMMAND,
  PARSING_KEY_LEN,
  PARSING_KEY,
  PARSING_VALUE_LEN,
  PARSING_VALUE,
  PARSING_FINISHED

} ParsingStage;

#endif __SERVER_UTILS_H__
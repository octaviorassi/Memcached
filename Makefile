# Compiler and flags
CC = gcc
CFLAGS = -Isrc -g

SRC_DIR = src
OBJ_DIR = obj
BIN_DIR = bin

SRC = $(wildcard src/**/*.c)
SRC := $(filter-out src/app/%.c, $(SRC)) # Saco las pruebas

OBJS = $(patsubst src/%.c, $(OBJ_DIR)/%.o, $(SRC)) # los objetos

OBJS_SERVER = $(filter-out $(OBJ_DIR)/server/server_starter%.o, $(OBJS)) 
OBJS_STARTER = $(filter $(OBJ_DIR)/server/server_starter%.o, $(OBJS)) 

SERVER  = $(BIN_DIR)/cache_server
STARTER = $(BIN_DIR)/server 

all: $(SERVER) $(STARTER) 

set-bind-privilege:
	@sudo setcap 'cap_net_bind_service=+ep' $(STARTER)
	
remove-bind-privilege:
	@sudo setcap 'cap_net_bind_service=+ep' $(STARTER)

$(SERVER): $(OBJS_SERVER)
	@mkdir -p $(BIN_DIR)
	$(CC) $(CFLAGS) -o $@ $^

$(STARTER): $(OBJS_STARTER)
	@mkdir -p $(BIN_DIR)
	$(CC) $(CFLAGS) -o $@ $^

$(OBJ_DIR)/%.o: src/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@
	
clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

.PHONY: all clean

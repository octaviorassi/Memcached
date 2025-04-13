# Compiler and flags
CC = gcc
ERLC = erlc
ERL = erl

ifeq ($(debug),yes)
	CFLAGS = -Isrc -Wall -Werror -Wextra -g 
else
	CFLAGS = -Isrc -Wall -Werror -Wextra
endif

SRC_DIR = src
OBJ_DIR = obj
BIN_DIR = bin

SRC = $(wildcard src/**/*.c)
SRC := $(filter-out src/app/%.c, $(SRC)) # Saco las pruebas
ERL_SRC = $(wildcard src/**/*.erl)

OBJS = $(patsubst src/%.c, $(OBJ_DIR)/%.o, $(SRC)) # los objetos

OBJS_SERVER = $(filter-out $(OBJ_DIR)/server/server_starter%.o, $(OBJS)) 
OBJS_STARTER = $(filter $(OBJ_DIR)/server/server_starter%.o, $(OBJS)) $(OBJ_DIR)/helpers/quit.o

SERVER  = $(BIN_DIR)/cache_server
STARTER = $(BIN_DIR)/server 

all: server client

server: $(SERVER) $(STARTER)

client:
	@mkdir -p $(BIN_DIR)
	@$(ERLC) -o $(BIN_DIR) $(ERL_SRC)

run-client:
	@$(ERL) -pa $(BIN_DIR)

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
	@rm -rf $(OBJ_DIR) $(BIN_DIR)
	@echo "All files cleaned!"
.PHONY: all clean

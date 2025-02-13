# Compiler and flags
CC = gcc
CFLAGS = -Isrc -g

# CFLAGS = -Wall -Wextra -O2 -Isrc

# Directories
SRC_DIR = src
OBJ_DIR = obj
BIN_DIR = bin

# Source files
SRC_CACHE = $(SRC_DIR)/cache/cache.c
SRC_CACHE_STATS = $(SRC_DIR)/cache/cache_stats.c
SRC_DYNALLOC = $(SRC_DIR)/dynalloc/dynalloc.c
SRC_LRU = $(SRC_DIR)/lru/lru.c
SRC_LRUNODE = $(SRC_DIR)/lru/lrunode.c
SRC_HASHMAP = $(SRC_DIR)/hashmap/hashnode.c
SRC_HELPERS = $(SRC_DIR)/helpers/results.c
SRC_ATOM_COUNTER = $(SRC_DIR)/helpers/atom_counter.c
SRC_APP = $(SRC_DIR)/app/main.c
SRC_CACHE_SERVER = $(SRC_DIR)/server/cache_server.c
SRC_CACHE_SERVER_UTILS = $(SRC_DIR)/server/cache_server_utils.c
SRC_SERVER_STARTER = $(SRC_DIR)/server/server_starter.c
SRC_SERVER_STARTER_UTILS = $(SRC_DIR)/server/server_starter_utils.c

# Object files
OBJ_CACHE = $(OBJ_DIR)/cache/cache.o
OBJ_CACHE_STATS = $(OBJ_DIR)/cache/cache_stats.o
OBJ_DYNALLOC = $(OBJ_DIR)/dynalloc/dynalloc.o
OBJ_LRU = $(OBJ_DIR)/lru/lru.o
OBJ_LRUNODE = $(OBJ_DIR)/lru/lrunode.o
OBJ_HASHMAP = $(OBJ_DIR)/hashmap/hashnode.o
OBJ_HELPERS = $(OBJ_DIR)/helpers/results.o
OBJ_ATOM_COUNTER = $(OBJ_DIR)/helpers/atom_counter.o
OBJ_APP = $(OBJ_DIR)/app/main.o
OBJ_CACHE_SERVER = $(SRC_DIR)/server/cache_server.o
OBJ_CACHE_SERVER_UTILS = $(SRC_DIR)/server/cache_server_utils.o
OBJ_SERVER_STARTER = $(SRC_DIR)/server/server_starter.o
OBJ_SERVER_STARTER_UTILS = $(SRC_DIR)/server/server_starter_utils.o

# All object files
OBJS = $(OBJ_CACHE) $(OBJ_CACHE_STATS) $(OBJ_DYNALLOC) $(OBJ_LRU) $(OBJ_LRUNODE) $(OBJ_HASHMAP) $(OBJ_HELPERS) $(OBJ_ATOM_COUNTER) $(OBJ_CACHE_SERVER) $(OBJ_CACHE_SERVER_UTILS) 

# Output executable
TARGET = $(BIN_DIR)/cache_server

# Default target
all: $(TARGET)

# Link the program
$(TARGET): $(OBJS)
	@mkdir -p $(BIN_DIR)
	$(CC) $(CFLAGS) -o $@ $^

# Compile cache.c
$(OBJ_CACHE): $(SRC_CACHE) $(SRC_DIR)/cache/cache_stats.h
	@mkdir -p $(OBJ_DIR)/cache
	$(CC) $(CFLAGS) -c -o $@ $<

# Compile cache_stats.c
$(OBJ_CACHE_STATS): $(SRC_CACHE_STATS) $(SRC_DIR)/cache/cache_stats.h $(SRC_DIR)/helpers/atom_counter.h
	@mkdir -p $(OBJ_DIR)/cache
	$(CC) $(CFLAGS) -c -o $@ $<

# Compile dynalloc.c
$(OBJ_DYNALLOC): $(SRC_DYNALLOC)
	@mkdir -p $(OBJ_DIR)/dynalloc
	$(CC) $(CFLAGS) -c -o $@ $<

# Compile lru.c
$(OBJ_LRU): $(SRC_LRU)
	@mkdir -p $(OBJ_DIR)/lru
	$(CC) $(CFLAGS) -c -o $@ $<

# Compile lrunode.c
$(OBJ_LRUNODE): $(SRC_LRUNODE)
	@mkdir -p $(OBJ_DIR)/lru
	$(CC) $(CFLAGS) -c -o $@ $<

# Compile hashnode.c
$(OBJ_HASHMAP): $(SRC_HASHMAP)
	@mkdir -p $(OBJ_DIR)/hashmap
	$(CC) $(CFLAGS) -c -o $@ $<

# Compile results.c
$(OBJ_HELPERS): $(SRC_HELPERS)
	@mkdir -p $(OBJ_DIR)/helpers
	$(CC) $(CFLAGS) -c -o $@ $<

# Compile atom_counter.c
$(OBJ_ATOM_COUNTER): $(SRC_ATOM_COUNTER) $(SRC_DIR)/helpers/atom_counter.h
	@mkdir -p $(OBJ_DIR)/helpers
	$(CC) $(CFLAGS) -c -o $@ $<

# Compile cache_server.c
$(OBJ_CACHE_SERVER): $(SRC_CACHE_SERVER) $(SRC_CACHE_SERVER_UTILS)
	@mkdir -p $(OBJ_DIR)/server
	$(CC) $(CFLAGS) -c -o $@ $<

# Compile cache_server_utils.c
$(OBJ_CACHE_SERVER_UTILS): $(SRC_CACHE_SERVER_UTILS)
	@mkdir -p $(OBJ_DIR)/server
	$(CC) $(CFLAGS) -c -o $@ $<

# # Compile main.c
# $(OBJ_APP): $(SRC_APP)
# 	@mkdir -p $(OBJ_DIR)/app
# 	$(CC) $(CFLAGS) -c -o $@ $<

# Clean up build files
clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

.PHONY: all clean
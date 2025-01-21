# Compiler
CC = gcc

# Compiler flags
# CFLAGS = -Wall -Wextra -I./src
CFLAGS = -I./src

# Directories
SRC_DIR = src
APP_DIR = $(SRC_DIR)/app
CACHE_DIR = $(SRC_DIR)/cache
HASHMAP_DIR = $(SRC_DIR)/hashmap
HELPERS_DIR = $(SRC_DIR)/helpers
LRU_DIR = $(SRC_DIR)/lru

# Source files
SRCS = $(APP_DIR)/main.c \
       $(CACHE_DIR)/cache.c \
       $(HASHMAP_DIR)/hashmap.c \
       $(HASHMAP_DIR)/hashnode.c \
       $(HELPERS_DIR)/results.c \
       $(LRU_DIR)/lru.c \
       $(LRU_DIR)/lrunode.c

# Object files
OBJS = $(SRCS:.c=.o)

# Executable
TARGET = my_project

# Default target
all: $(TARGET)

# Link object files to create the executable
$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

# Compile source files to object files
$(APP_DIR)/main.o: $(APP_DIR)/main.c
	$(CC) $(CFLAGS) -c $< -o $@

$(CACHE_DIR)/cache.o: $(CACHE_DIR)/cache.c $(CACHE_DIR)/cache.h $(LRU_DIR)/lru.h $(HASHMAP_DIR)/hashmap.h
	$(CC) $(CFLAGS) -c $< -o $@

$(HASHMAP_DIR)/hashmap.o: $(HASHMAP_DIR)/hashmap.c $(HASHMAP_DIR)/hashmap.h $(HASHMAP_DIR)/hashnode.h $(HELPERS_DIR)/results.h
	$(CC) $(CFLAGS) -c $< -o $@

$(HASHMAP_DIR)/hashnode.o: $(HASHMAP_DIR)/hashnode.c $(HASHMAP_DIR)/hashnode.h $(LRU_DIR)/lrunode.h
	$(CC) $(CFLAGS) -c $< -o $@

$(HELPERS_DIR)/results.o: $(HELPERS_DIR)/results.c $(HELPERS_DIR)/results.h
	$(CC) $(CFLAGS) -c $< -o $@

$(LRU_DIR)/lru.o: $(LRU_DIR)/lru.c $(LRU_DIR)/lru.h $(LRU_DIR)/lrunode.h
	$(CC) $(CFLAGS) -c $< -o $@

$(LRU_DIR)/lrunode.o: $(LRU_DIR)/lrunode.c $(LRU_DIR)/lrunode.h $(HASHMAP_DIR)/hashnode.h
	$(CC) $(CFLAGS) -c $< -o $@

# Clean up build files
clean:
	rm -f $(OBJS) $(TARGET)

.PHONY: all clean
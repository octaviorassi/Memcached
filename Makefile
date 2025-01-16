# Compiler and flags
CC = gcc
CFLAGS = -std=c99 -g  # Add debugging info with -g if needed

# Directories
SRC_DIR = src
BUILD_DIR = build

# Source files
SRCS = $(SRC_DIR)/cache/cache.c $(SRC_DIR)/hash/hash.c $(SRC_DIR)/lru/lru.c $(SRC_DIR)/nodes/nodes.c

# Object files (generated from source files)
OBJS = $(SRCS:$(SRC_DIR)/%.c=$(BUILD_DIR)/%.o)

# Target executable
TARGET = my_program

# Rules

# Default target (build the target executable)
all: $(TARGET)

# Linking the object files into the final executable
$(TARGET): $(OBJS)
	$(CC) $(OBJS) -o $(TARGET)

# Rule to compile each .c file into a .o file
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(BUILD_DIR)  # Create build directory if it doesn't exist
	$(CC) $(CFLAGS) -I$(SRC_DIR)/lru -I$(SRC_DIR)/hash -I$(SRC_DIR)/nodes -I$(SRC_DIR)/cache -c $< -o $@

# Clean the build directory and remove the target executable
clean:
	rm -rf $(BUILD_DIR) $(TARGET)

# Phony targets (not actual files)
.PHONY: all clean

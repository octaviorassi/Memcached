# Compiler settings
CC = gcc
CFLAGS = -std=c99 -g

# Directories
SRC_DIR = src
BUILD_DIR = build
OBJ_DIR = $(BUILD_DIR)/cache
APP_DIR = $(SRC_DIR)/app
OBJ_FILES = $(OBJ_DIR)/cache.o $(BUILD_DIR)/app/main.o  # Add main.o from src/app

# Include paths
INCLUDES = -Isrc/lru -Isrc/hash -Isrc/nodes -Isrc/cache -Isrc

# Target executable
TARGET = memcached

# Default target
all: $(TARGET)

# Rule to compile cache files
$(OBJ_DIR)/%.o: $(SRC_DIR)/cache/%.c
	@mkdir -p $(OBJ_DIR)  # Create the directory if it doesn't exist
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

# Rule to compile main.c from src/app
$(BUILD_DIR)/app/main.o: $(APP_DIR)/main.c
	@mkdir -p $(BUILD_DIR)/app  # Create the directory if it doesn't exist
	$(CC) $(CFLAGS) $(INCLUDES) -c $(APP_DIR)/main.c -o $(BUILD_DIR)/app/main.o

# Linking rule to create the executable
$(TARGET): $(OBJ_FILES)
	$(CC) $(OBJ_FILES) -o $(TARGET)

# Clean up
clean:
	rm -rf $(BUILD_DIR) $(TARGET)

.PHONY: all clean

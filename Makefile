# AstraLisp OS Production Build System
# Comprehensive Makefile with dependency tracking, parallel builds, and Windows support

# Detect OS
UNAME_S := $(shell uname -s 2>/dev/null || echo "Windows")
ifeq ($(UNAME_S),Windows)
    DETECTED_OS := Windows
    SHELL := cmd.exe
    RM := del /Q /F
    MKDIR := mkdir
    PATHSEP := \\
    TOOLCHAIN_PREFIX ?= $(USERPROFILE)\\opt\\powerpc64le-linux-gnu
else
    DETECTED_OS := $(shell uname -s)
    RM := rm -rf
    MKDIR := mkdir -p
    PATHSEP := /
    TOOLCHAIN_PREFIX ?= $(HOME)/opt/powerpc64le-linux-gnu
endif

# Toolchain configuration
CC := $(TOOLCHAIN_PREFIX)/bin/powerpc64le-linux-gnu-gcc
CXX := $(TOOLCHAIN_PREFIX)/bin/powerpc64le-linux-gnu-g++
AS := $(TOOLCHAIN_PREFIX)/bin/powerpc64le-linux-gnu-as
LD := $(TOOLCHAIN_PREFIX)/bin/powerpc64le-linux-gnu-ld
OBJCOPY := $(TOOLCHAIN_PREFIX)/bin/powerpc64le-linux-gnu-objcopy
OBJDUMP := $(TOOLCHAIN_PREFIX)/bin/powerpc64le-linux-gnu-objdump
STRIP := $(TOOLCHAIN_PREFIX)/bin/powerpc64le-linux-gnu-strip
AR := $(TOOLCHAIN_PREFIX)/bin/powerpc64le-linux-gnu-ar
RANLIB := $(TOOLCHAIN_PREFIX)/bin/powerpc64le-linux-gnu-ranlib

# Build configuration
BUILD_DIR := build
KERNEL_DIR := kernel
BOOTLOADER_DIR := bootloader
RUNTIME_DIR := runtime
FILESYSTEM_DIR := filesystem
NETWORK_DIR := network
UI_DIR := ui
USERLAND_DIR := userland

# Compiler flags
CFLAGS := -m64 -msoft-float -mno-altivec -mno-vsx -ffreestanding -fno-stack-protector \
          -fno-common -fno-builtin -Wall -Wextra -Werror -std=c11 -O2 -g
CXXFLAGS := $(CFLAGS) -std=c++17 -fno-exceptions -fno-rtti
ASFLAGS := -m64 -msoft-float
LDFLAGS := -m64 -nostdlib -static -T linker.ld

# Include directories
INCLUDES := -I$(KERNEL_DIR) -I$(KERNEL_DIR)/hal -I$(KERNEL_DIR)/mm \
            -I$(KERNEL_DIR)/process -I$(KERNEL_DIR)/interrupt -I$(KERNEL_DIR)/driver \
            -I$(RUNTIME_DIR)/lisp -I$(RUNTIME_DIR)/gc


# Source files
KERNEL_SOURCES := $(shell find $(KERNEL_DIR) -name "*.c" -o -name "*.cpp" 2>/dev/null)
KERNEL_ASM := $(shell find $(KERNEL_DIR) -name "*.asm" -o -name "*.S" 2>/dev/null)
RUNTIME_SOURCES := $(shell find $(RUNTIME_DIR) -name "*.c" 2>/dev/null)
BOOTLOADER_SOURCES := $(shell find $(BOOTLOADER_DIR) -name "*.c" -o -name "*.cpp" 2>/dev/null)
BOOTLOADER_ASM := $(shell find $(BOOTLOADER_DIR) -name "*.asm" -o -name "*.S" 2>/dev/null)

# Object files
KERNEL_OBJECTS := $(KERNEL_SOURCES:%.c=$(BUILD_DIR)/%.o) $(KERNEL_SOURCES:%.cpp=$(BUILD_DIR)/%.o) \
                 $(KERNEL_ASM:%.asm=$(BUILD_DIR)/%.o) $(KERNEL_ASM:%.S=$(BUILD_DIR)/%.o) \
                 $(RUNTIME_SOURCES:%.c=$(BUILD_DIR)/%.o)

BOOTLOADER_OBJECTS := $(BOOTLOADER_SOURCES:%.c=$(BUILD_DIR)/%.o) \
                     $(BOOTLOADER_SOURCES:%.cpp=$(BUILD_DIR)/%.o) \
                     $(BOOTLOADER_ASM:%.asm=$(BUILD_DIR)/%.o) \
                     $(BOOTLOADER_ASM:%.S=$(BUILD_DIR)/%.o)

# Targets
.PHONY: all clean toolchain kernel bootloader iso test help

all: toolchain kernel bootloader iso

help:
	@echo "AstraLisp OS Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all          - Build everything (toolchain, kernel, bootloader, ISO)"
	@echo "  toolchain    - Install/verify PowerISA cross-compilation toolchain"
	@echo "  kernel       - Build kernel"
	@echo "  bootloader   - Build bootloader"
	@echo "  iso          - Create bootable ISO"
	@echo "  clean        - Clean build artifacts"
	@echo "  test         - Run tests"
	@echo ""
	@echo "Configuration:"
	@echo "  TOOLCHAIN_PREFIX=$(TOOLCHAIN_PREFIX)"
	@echo "  BUILD_DIR=$(BUILD_DIR)"
	@echo "  DETECTED_OS=$(DETECTED_OS)"

toolchain:
	@echo "Checking toolchain..."
	@if [ ! -f "$(CC)" ]; then \
		echo "Toolchain not found at $(TOOLCHAIN_PREFIX)"; \
		echo "Run: ./toolchain/install-powerisa.sh"; \
		exit 1; \
	fi
	@echo "Toolchain OK: $(CC)"

# Dependency tracking
-include $(KERNEL_OBJECTS:.o=.d)
-include $(BOOTLOADER_OBJECTS:.o=.d)

# Compilation rules
$(BUILD_DIR)/%.o: %.c
	@$(MKDIR) $(dir $@)
	$(CC) $(CFLAGS) $(INCLUDES) -MMD -MP -c $< -o $@

$(BUILD_DIR)/%.o: %.cpp
	@$(MKDIR) $(dir $@)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -MMD -MP -c $< -o $@

$(BUILD_DIR)/%.o: %.asm
	@$(MKDIR) $(dir $@)
	$(AS) $(ASFLAGS) $< -o $@

$(BUILD_DIR)/%.o: %.S
	@$(MKDIR) $(dir $@)
	$(CC) $(CFLAGS) $(INCLUDES) -MMD -MP -c $< -o $@

# Kernel build
kernel: toolchain $(BUILD_DIR)/kernel.elf
	@echo "Kernel built: $(BUILD_DIR)/kernel.elf"

$(BUILD_DIR)/kernel.elf: $(KERNEL_OBJECTS) linker.ld
	$(LD) $(LDFLAGS) -o $@ $(KERNEL_OBJECTS)
	$(OBJCOPY) -O binary $@ $(BUILD_DIR)/kernel.bin

# Bootloader build
bootloader: toolchain $(BUILD_DIR)/bootloader.elf
	@echo "Bootloader built: $(BUILD_DIR)/bootloader.elf"

$(BUILD_DIR)/bootloader.elf: $(BOOTLOADER_OBJECTS) bootloader-linker.ld
	$(LD) $(LDFLAGS) -T bootloader-linker.ld -o $@ $(BOOTLOADER_OBJECTS)

# ISO creation
iso: kernel bootloader
	@echo "Creating bootable ISO..."
	@bash scripts/create-iso.sh

# Testing
test:
	@echo "Running tests..."
	@cd tests && $(MAKE) test

# Clean
clean:
	$(RM) $(BUILD_DIR)
	@echo "Build artifacts cleaned"

# Parallel builds
.NOTPARALLEL: toolchain iso

# Windows path handling
ifeq ($(DETECTED_OS),Windows)
    # Convert Windows paths for tools that expect Unix paths
    CC := $(subst \,/,$(CC))
    CXX := $(subst \,/,$(CXX))
    AS := $(subst \,/,$(AS))
    LD := $(subst \,/,$(LD))
endif

#!/bin/bash
# AstraLisp OS ISO Creation Script

set -e

BUILD_DIR="${BUILD_DIR:-build}"
ISO_DIR="$BUILD_DIR/iso"
ISO_FILE="$BUILD_DIR/astralisp.iso"

echo "Creating AstraLisp OS ISO..."

# Create ISO directory structure
mkdir -p "$ISO_DIR/boot/grub"
mkdir -p "$ISO_DIR/boot/kernel"
mkdir -p "$ISO_DIR/initrd"

# Copy kernel
if [ -f "$BUILD_DIR/kernel.elf" ]; then
    cp "$BUILD_DIR/kernel.elf" "$ISO_DIR/boot/kernel/"
else
    echo "Error: kernel.elf not found"
    exit 1
fi

# Copy bootloader
if [ -f "$BUILD_DIR/bootloader.elf" ]; then
    cp "$BUILD_DIR/bootloader.elf" "$ISO_DIR/boot/"
fi

# Copy GRUB configuration
if [ -f "grub.cfg" ]; then
    cp grub.cfg "$ISO_DIR/boot/grub/"
else
    echo "Error: grub.cfg not found"
    exit 1
fi

# Create initrd (placeholder)
touch "$ISO_DIR/initrd/init"

# Create ISO
if command -v grub-mkrescue &> /dev/null; then
    grub-mkrescue -o "$ISO_FILE" "$ISO_DIR"
    echo "ISO created: $ISO_FILE"
elif command -v xorriso &> /dev/null; then
    xorriso -as mkisofs -R -b boot/grub/eltorito.img -no-emul-boot -boot-load-size 4 -boot-info-table -o "$ISO_FILE" "$ISO_DIR"
    echo "ISO created: $ISO_FILE"
else
    echo "Error: grub-mkrescue or xorriso not found"
    exit 1
fi

echo "ISO creation complete!"

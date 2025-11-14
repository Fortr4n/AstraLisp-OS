#!/bin/bash
# AstraLisp OS QEMU Test Script

set -e

BUILD_DIR="${BUILD_DIR:-build}"
ISO_FILE="$BUILD_DIR/astralisp.iso"
QEMU_ARCH="${QEMU_ARCH:-ppc64}"

echo "Testing AstraLisp OS in QEMU..."

if [ ! -f "$ISO_FILE" ]; then
    echo "Error: ISO file not found: $ISO_FILE"
    echo "Please build the ISO first: make iso"
    exit 1
fi

# Check for QEMU
if ! command -v qemu-system-ppc64 &> /dev/null && ! command -v qemu-system-ppc64le &> /dev/null; then
    echo "Error: QEMU not found"
    echo "Please install QEMU: sudo apt-get install qemu-system-ppc"
    exit 1
fi

# Determine QEMU command
if command -v qemu-system-ppc64le &> /dev/null; then
    QEMU_CMD="qemu-system-ppc64le"
elif command -v qemu-system-ppc64 &> /dev/null; then
    QEMU_CMD="qemu-system-ppc64"
else
    echo "Error: No suitable QEMU found"
    exit 1
fi

echo "Starting QEMU with ISO: $ISO_FILE"
echo "Press Ctrl+C to stop QEMU"
echo ""

# Run QEMU
$QEMU_CMD \
    -M pseries \
    -m 512M \
    -cdrom "$ISO_FILE" \
    -serial stdio \
    -nographic \
    -monitor none

echo ""
echo "QEMU test complete"

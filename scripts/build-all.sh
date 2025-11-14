#!/bin/bash
# AstraLisp OS Complete Build Script

set -e

echo "=========================================="
echo "AstraLisp OS Complete Build"
echo "=========================================="
echo ""

# Check for toolchain
if [ ! -f "$HOME/opt/powerpc64le-linux-gnu/bin/powerpc64le-linux-gnu-gcc" ]; then
    echo "Toolchain not found. Installing..."
    cd toolchain
    ./install-powerisa.sh
    cd ..
    source ~/opt/powerpc64le-linux-gnu/env.sh
fi

# Source toolchain environment
if [ -f "$HOME/opt/powerpc64le-linux-gnu/env.sh" ]; then
    source ~/opt/powerpc64le-linux-gnu/env.sh
fi

# Build everything
echo "Building kernel..."
make kernel

echo "Building bootloader..."
make bootloader

echo "Creating ISO..."
make iso

echo ""
echo "=========================================="
echo "Build complete!"
echo "ISO: build/astralisp.iso"
echo "=========================================="

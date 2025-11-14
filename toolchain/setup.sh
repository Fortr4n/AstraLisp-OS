#!/bin/bash
# AstraLisp OS Toolchain Setup Script for Linux/WSL2
# This script sets up the complete PowerISA cross-compilation toolchain

set -e

echo "AstraLisp OS Toolchain Setup for Linux/WSL2"
echo "==========================================="

# Detect distribution
if [ -f /etc/os-release ]; then
    . /etc/os-release
    DISTRO=$ID
    VERSION=$VERSION_ID
    echo "Detected: $PRETTY_NAME"
else
    echo "WARNING: Cannot detect Linux distribution"
    DISTRO="unknown"
fi

# Function to install packages on Debian/Ubuntu
install_debian() {
    echo ""
    echo "Installing packages for Debian/Ubuntu..."
    sudo apt-get update
    sudo apt-get install -y \
        build-essential \
        gcc-powerpc64le-linux-gnu \
        g++-powerpc64le-linux-gnu \
        binutils-powerpc64le-linux-gnu \
        qemu-system-ppc \
        qemu-utils \
        nasm \
        make \
        git \
        python3 \
        genisoimage \
        xorriso \
        grub-pc-bin \
        grub-common
}

# Function to install packages on Fedora/RHEL
install_fedora() {
    echo ""
    echo "Installing packages for Fedora/RHEL..."
    sudo dnf install -y \
        gcc \
        gcc-c++ \
        gcc-powerpc64le-linux-gnu \
        binutils-powerpc64le-linux-gnu \
        qemu-system-ppc \
        nasm \
        make \
        git \
        python3 \
        genisoimage \
        xorriso \
        grub2
}

# Function to install packages on Arch
install_arch() {
    echo ""
    echo "Installing packages for Arch Linux..."
    sudo pacman -S --noconfirm \
        base-devel \
        powerpc64le-linux-gnu-gcc \
        powerpc64le-linux-gnu-binutils \
        qemu-system-ppc \
        nasm \
        make \
        git \
        python \
        cdrtools \
        grub
}

# Install based on distribution
case $DISTRO in
    debian|ubuntu)
        install_debian
        ;;
    fedora|rhel|centos)
        install_fedora
        ;;
    arch|manjaro)
        install_arch
        ;;
    *)
        echo "WARNING: Unknown distribution. Please install manually:"
        echo "  - GCC cross-compiler for powerpc64le-linux-gnu"
        echo "  - QEMU with PowerISA support"
        echo "  - NASM or GAS"
        echo "  - Make, Git, Python3"
        echo "  - ISO creation tools (genisoimage/xorriso)"
        echo "  - GRUB"
        ;;
esac

# Check for Lisp implementation
echo ""
echo "Checking for Lisp implementation..."
LISP_FOUND=0

if command -v sbcl &> /dev/null; then
    echo "Found: SBCL"
    sbcl --version | head -1
    LISP_FOUND=1
fi

if command -v clisp &> /dev/null; then
    echo "Found: CLISP"
    clisp --version | head -1
    LISP_FOUND=1
fi

if [ $LISP_FOUND -eq 0 ]; then
    echo "WARNING: No Lisp implementation found (SBCL or CLISP)"
    echo "  Install SBCL or CLISP for bootstrapping"
    case $DISTRO in
        debian|ubuntu)
            echo "  Run: sudo apt-get install sbcl"
            ;;
        fedora|rhel|centos)
            echo "  Run: sudo dnf install sbcl"
            ;;
        arch|manjaro)
            echo "  Run: sudo pacman -S sbcl"
            ;;
    esac
fi

# Verify cross-compiler
echo ""
echo "Verifying PowerISA cross-compiler..."
if command -v powerpc64le-linux-gnu-gcc &> /dev/null; then
    echo "Found: PowerISA cross-compiler"
    powerpc64le-linux-gnu-gcc --version | head -1
    
    # Test compilation
    echo "Testing cross-compiler..."
    cat > /tmp/test_ppc.c << 'EOF'
int main() { return 0; }
EOF
    if powerpc64le-linux-gnu-gcc -o /tmp/test_ppc /tmp/test_ppc.c 2>/dev/null; then
        echo "Cross-compiler test: PASSED" 
        rm -f /tmp/test_ppc /tmp/test_ppc.c
    else
        echo "Cross-compiler test: FAILED"
        rm -f /tmp/test_ppc /tmp/test_ppc.c
        exit 1
    fi
else
    echo "ERROR: PowerISA cross-compiler not found"
    exit 1
fi

# Verify QEMU
echo ""
echo "Verifying QEMU..."
if command -v qemu-system-ppc64 &> /dev/null; then
    echo "Found: QEMU PowerISA support"
    qemu-system-ppc64 --version | head -1
else
    echo "WARNING: QEMU with PowerISA support not found"
    echo "  Install QEMU for testing the OS"
fi

echo ""
echo "Toolchain setup complete!"
echo "You can now run 'make toolchain' to verify everything is ready"


#!/bin/bash
# AstraLisp OS PowerISA Cross-Compilation Toolchain Installer
# Supports Windows 11 Pro (WSL2) and native Linux

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOOLCHAIN_DIR="${TOOLCHAIN_DIR:-$HOME/opt/powerpc64le-linux-gnu}"
PREFIX="${TOOLCHAIN_DIR}"

echo "=========================================="
echo "AstraLisp OS PowerISA Toolchain Installer"
echo "=========================================="
echo ""

# Detect OS
if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "cygwin" || -n "$WSL_DISTRO_NAME" ]]; then
    IS_WINDOWS=true
    echo "Detected Windows/WSL2 environment"
else
    IS_WINDOWS=false
    echo "Detected Linux environment"
fi

# Check for required tools
check_dependencies() {
    local missing=()
    
    for cmd in wget curl make gcc g++ bison flex; do
        if ! command -v $cmd &> /dev/null; then
            missing+=($cmd)
        fi
    done
    
    if [ ${#missing[@]} -ne 0 ]; then
        echo "Missing dependencies: ${missing[*]}"
        echo "Please install them first."
        exit 1
    fi
}

# Install binutils
install_binutils() {
    local version="2.42"
    local url="https://ftp.gnu.org/gnu/binutils/binutils-${version}.tar.xz"
    
    echo "Installing binutils ${version}..."
    
    cd "$SCRIPT_DIR"
    if [ ! -f "binutils-${version}.tar.xz" ]; then
        wget "$url"
    fi
    
    if [ ! -d "binutils-${version}" ]; then
        tar -xf "binutils-${version}.tar.xz"
    fi
    
    cd "binutils-${version}"
    mkdir -p build && cd build
    
    ../configure \
        --target=powerpc64le-linux-gnu \
        --prefix="$PREFIX" \
        --disable-nls \
        --with-sysroot="$PREFIX/sysroot" \
        --enable-shared \
        --enable-plugins
    
    make -j$(nproc)
    make install
    
    echo "binutils installed successfully"
}

# Install GCC
install_gcc() {
    local version="13.2.0"
    local url="https://ftp.gnu.org/gnu/gcc/gcc-${version}/gcc-${version}.tar.xz"
    
    echo "Installing GCC ${version}..."
    
    cd "$SCRIPT_DIR"
    if [ ! -f "gcc-${version}.tar.xz" ]; then
        wget "$url"
    fi
    
    if [ ! -d "gcc-${version}" ]; then
        tar -xf "gcc-${version}.tar.xz"
        cd "gcc-${version}"
        ./contrib/download_prerequisites
    else
        cd "gcc-${version}"
    fi
    
    mkdir -p build && cd build
    
    ../configure \
        --target=powerpc64le-linux-gnu \
        --prefix="$PREFIX" \
        --disable-nls \
        --enable-languages=c,c++ \
        --without-headers \
        --with-newlib \
        --with-sysroot="$PREFIX/sysroot" \
        --enable-shared \
        --enable-threads=posix \
        --enable-__cxa_atexit \
        --enable-long-long \
        --with-multilib-list=m64 \
        --disable-libsanitizer
    
    make -j$(nproc) all-gcc all-target-libgcc
    make install-gcc install-target-libgcc
    
    echo "GCC installed successfully"
}

# Install Clang (optional, via LLVM)
install_clang() {
    local version="18.1.0"
    local url="https://github.com/llvm/llvm-project/releases/download/llvmorg-${version}/llvm-${version}.src.tar.xz"
    
    echo "Installing Clang ${version} (optional)..."
    
    cd "$SCRIPT_DIR"
    if [ ! -f "llvm-${version}.src.tar.xz" ]; then
        wget "$url"
    fi
    
    if [ ! -d "llvm-${version}.src" ]; then
        tar -xf "llvm-${version}.src.tar.xz"
    fi
    
    cd "llvm-${version}.src"
    mkdir -p build && cd build
    
    cmake .. \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX="$PREFIX" \
        -DLLVM_TARGETS_TO_BUILD="PowerPC" \
        -DLLVM_DEFAULT_TARGET_TRIPLE=powerpc64le-linux-gnu \
        -DCMAKE_C_COMPILER=gcc \
        -DCMAKE_CXX_COMPILER=g++ \
        -DLLVM_ENABLE_PROJECTS="clang;lld" \
        -DLLVM_ENABLE_ASSERTIONS=OFF
    
    cmake --build . -j$(nproc)
    cmake --install .
    
    echo "Clang installed successfully"
}

# Create sysroot structure
create_sysroot() {
    echo "Creating sysroot structure..."
    
    mkdir -p "$PREFIX/sysroot/usr/include"
    mkdir -p "$PREFIX/sysroot/usr/lib"
    mkdir -p "$PREFIX/sysroot/lib"
    
    echo "Sysroot created"
}

# Setup environment script
create_env_script() {
    local env_script="$PREFIX/env.sh"
    
    cat > "$env_script" <<EOF
#!/bin/bash
# AstraLisp OS PowerISA Toolchain Environment

export TOOLCHAIN_PREFIX="$PREFIX"
export PATH="\$TOOLCHAIN_PREFIX/bin:\$PATH"
export CC=powerpc64le-linux-gnu-gcc
export CXX=powerpc64le-linux-gnu-g++
export AR=powerpc64le-linux-gnu-ar
export AS=powerpc64le-linux-gnu-as
export LD=powerpc64le-linux-gnu-ld
export OBJCOPY=powerpc64le-linux-gnu-objcopy
export OBJDUMP=powerpc64le-linux-gnu-objdump
export STRIP=powerpc64le-linux-gnu-strip
export RANLIB=powerpc64le-linux-gnu-ranlib
export CFLAGS="-m64 -msoft-float -mno-altivec -mno-vsx"
export CXXFLAGS="\$CFLAGS"
export LDFLAGS="-m64"

echo "PowerISA toolchain environment activated"
echo "Toolchain prefix: \$TOOLCHAIN_PREFIX"
EOF
    
    chmod +x "$env_script"
    echo "Environment script created at: $env_script"
    echo "Source it with: source $env_script"
}

# Main installation
main() {
    check_dependencies
    
    echo "Toolchain will be installed to: $PREFIX"
    echo "Press Enter to continue or Ctrl+C to cancel..."
    read
    
    mkdir -p "$PREFIX"
    
    install_binutils
    install_gcc
    create_sysroot
    create_env_script
    
    echo ""
    echo "=========================================="
    echo "Toolchain installation complete!"
    echo "=========================================="
    echo ""
    echo "To use the toolchain, run:"
    echo "  source $PREFIX/env.sh"
    echo ""
    echo "Or add to your ~/.bashrc:"
    echo "  source $PREFIX/env.sh"
    echo ""
}

main "$@"

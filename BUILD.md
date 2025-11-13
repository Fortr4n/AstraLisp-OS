# AstraLisp OS Build Instructions

## Prerequisites

### Windows 11 Pro

1. **Install WSL2** (recommended):
   ```powershell
   wsl --install
   ```

2. **Or use native Windows** (requires additional setup)

### Linux/WSL2

- GCC or Clang
- Make
- wget or curl
- tar
- grub-mkrescue (for ISO creation)
- QEMU (for testing)

## Building

### Step 1: Install Toolchain

**On WSL2/Linux:**
```bash
cd toolchain
./install-powerisa.sh
source ~/opt/powerpc64le-linux-gnu/env.sh
```

**On Windows (PowerShell):**
```powershell
cd toolchain
.\install-powerisa.ps1
```

### Step 2: Build Kernel

```bash
make kernel
```

### Step 3: Build Bootloader

```bash
make bootloader
```

### Step 4: Create ISO

```bash
make iso
```

The ISO will be created at `build/astralisp.iso`.

## Testing

### Run Unit Tests

```bash
make test
```

### Test in QEMU

```bash
./tests/qemu/test-qemu.sh
```

Or manually:
```bash
qemu-system-ppc64le -M pseries -m 512M -cdrom build/astralisp.iso -serial stdio -nographic
```

## Project Structure

```
astralisp-os/
├── bootloader/          # Multiboot 2.0 bootloader
├── kernel/              # Kernel source
│   ├── asm/            # Assembly routines
│   ├── hal/            # Hardware abstraction layer
│   ├── mm/             # Memory management
│   ├── process/        # Process and thread management
│   ├── interrupt/      # Interrupt handling
│   └── driver/         # Device drivers
├── runtime/             # Lisp runtime
│   ├── jit/            # JIT compiler
│   ├── gc/             # Garbage collector
│   └── ffi/            # Foreign function interface
├── filesystem/          # LFSX filesystem
├── network/             # TCP/IP stack
├── ui/                  # AstraUI framework
├── userland/            # Userland programs
├── tests/               # Test suite
└── toolchain/          # Cross-compilation toolchain
```

## Troubleshooting

### Toolchain not found

Make sure to source the environment script:
```bash
source ~/opt/powerpc64le-linux-gnu/env.sh
```

### Build fails

1. Check that all dependencies are installed
2. Verify toolchain is in PATH
3. Clean and rebuild: `make clean && make`

### QEMU not working

Install QEMU:
```bash
sudo apt-get install qemu-system-ppc
```

## Next Steps

1. Implement full device drivers
2. Complete filesystem implementation
3. Finish JIT compiler backend
4. Implement full network stack
5. Complete UI framework

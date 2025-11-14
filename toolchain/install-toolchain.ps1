# AstraLisp OS Automated Toolchain Installation for Windows
# Installs PowerISA cross-compilation toolchain

param(
    [switch]$UseWSL2,
    [string]$WSLDistro = "Ubuntu-22.04"
)

$ErrorActionPreference = "Stop"

Write-Host "AstraLisp OS Toolchain Installation" -ForegroundColor Cyan
Write-Host "====================================" -ForegroundColor Cyan

if ($UseWSL2) {
    Write-Host "`nInstalling toolchain in WSL2..." -ForegroundColor Yellow
    wsl -d $WSLDistro -e bash -c "cd $(Convert-Path .) && bash toolchain/setup.sh"
    exit $LASTEXITCODE
}

Write-Host "`nNative Windows toolchain installation..." -ForegroundColor Yellow
Write-Host "For best results, we recommend using WSL2:" -ForegroundColor Yellow
Write-Host "  .\toolchain\install-toolchain.ps1 -UseWSL2" -ForegroundColor Cyan
Write-Host "`nFor native Windows, you need to manually install:" -ForegroundColor Yellow
Write-Host "  1. MinGW-w64 or MSYS2 with GCC" -ForegroundColor White
Write-Host "  2. PowerISA cross-compiler (may need to build from source)" -ForegroundColor White
Write-Host "  3. QEMU for Windows" -ForegroundColor White
Write-Host "  4. SBCL or CLISP for Windows" -ForegroundColor White
Write-Host "  5. NASM assembler" -ForegroundColor White
Write-Host "  6. GNU Make for Windows" -ForegroundColor White
Write-Host "`nDownload links:" -ForegroundColor Yellow
Write-Host "  - MSYS2: https://www.msys2.org/" -ForegroundColor Cyan
Write-Host "  - QEMU: https://www.qemu.org/download/#windows" -ForegroundColor Cyan
Write-Host "  - SBCL: https://www.sbcl.org/platform-table.html" -ForegroundColor Cyan
Write-Host "  - NASM: https://www.nasm.us/" -ForegroundColor Cyan

$response = Read-Host "`nDo you want to open download pages? (y/n)"
if ($response -eq 'y' -or $response -eq 'Y') {
    Start-Process "https://www.msys2.org/"
    Start-Process "https://www.qemu.org/download/#windows"
    Start-Process "https://www.sbcl.org/platform-table.html"
}


# AstraLisp OS Toolchain Setup Script for Windows
# This script sets up the complete PowerISA cross-compilation toolchain

param(
    [switch]$UseWSL2,
    [string]$WSLDistro = "Ubuntu-22.04"
)

$ErrorActionPreference = "Stop"

Write-Host "AstraLisp OS Toolchain Setup for Windows" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan

# Check if running in WSL2 mode
if ($UseWSL2) {
    Write-Host "`nUsing WSL2 fallback mode..." -ForegroundColor Yellow
    $wslPath = wsl -d $WSLDistro -e pwd 2>$null
    if ($LASTEXITCODE -ne 0) {
        Write-Host "ERROR: WSL2 not available or distro '$WSLDistro' not found" -ForegroundColor Red
        exit 1
    }
    Write-Host "WSL2 distro '$WSLDistro' found. Running setup script in WSL2..." -ForegroundColor Green
    wsl -d $WSLDistro -e bash -c "cd $(Convert-Path .) && bash toolchain/setup.sh"
    exit $LASTEXITCODE
}

# Native Windows setup
Write-Host "`nSetting up native Windows toolchain..." -ForegroundColor Yellow

# Check for required tools
$requiredTools = @{
    "git" = "Git"
    "make" = "GNU Make"
    "gcc" = "GCC or MinGW"
}

$missingTools = @()
foreach ($tool in $requiredTools.Keys) {
    $found = Get-Command $tool -ErrorAction SilentlyContinue
    if (-not $found) {
        $missingTools += $requiredTools[$tool]
    } else {
        Write-Host "Found: $($requiredTools[$tool])" -ForegroundColor Green
    }
}

if ($missingTools.Count -gt 0) {
    Write-Host "`nMissing required tools:" -ForegroundColor Red
    $missingTools | ForEach-Object { Write-Host "  - $_" -ForegroundColor Red }
    Write-Host "`nPlease install missing tools or use -UseWSL2 flag for WSL2 setup" -ForegroundColor Yellow
    exit 1
}

# Check for Lisp implementation
Write-Host "`nChecking for Lisp implementation..." -ForegroundColor Yellow
$lispFound = $false

# Check for SBCL
$sbcl = Get-Command sbcl -ErrorAction SilentlyContinue
if ($sbcl) {
    Write-Host "Found: SBCL" -ForegroundColor Green
    $sbclVersion = & sbcl --version 2>&1 | Select-Object -First 1
    Write-Host "  Version: $sbclVersion" -ForegroundColor Gray
    $lispFound = $true
}

# Check for CLISP
$clisp = Get-Command clisp -ErrorAction SilentlyContinue
if ($clisp) {
    Write-Host "Found: CLISP" -ForegroundColor Green
    $clispVersion = & clisp --version 2>&1 | Select-Object -First 1
    Write-Host "  Version: $clispVersion" -ForegroundColor Gray
    $lispFound = $true
}

if (-not $lispFound) {
    Write-Host "WARNING: No Lisp implementation found (SBCL or CLISP)" -ForegroundColor Yellow
    Write-Host "  You may need to install SBCL or CLISP for bootstrapping" -ForegroundColor Yellow
}

# Check for QEMU
Write-Host "`nChecking for QEMU..." -ForegroundColor Yellow
$qemu = Get-Command qemu-system-ppc64 -ErrorAction SilentlyContinue
if ($qemu) {
    Write-Host "Found: QEMU PowerISA support" -ForegroundColor Green
    $qemuVersion = & qemu-system-ppc64 --version 2>&1 | Select-Object -First 1
    Write-Host "  Version: $qemuVersion" -ForegroundColor Gray
} else {
    Write-Host "WARNING: QEMU with PowerISA support not found" -ForegroundColor Yellow
    Write-Host "  Install QEMU for testing the OS" -ForegroundColor Yellow
}

# Check for cross-compiler
Write-Host "`nChecking for PowerISA cross-compiler..." -ForegroundColor Yellow
$crossGcc = Get-Command powerpc64le-linux-gnu-gcc -ErrorAction SilentlyContinue
if ($crossGcc) {
    Write-Host "Found: PowerISA cross-compiler" -ForegroundColor Green
    $gccVersion = & powerpc64le-linux-gnu-gcc --version 2>&1 | Select-Object -First 1
    Write-Host "  Version: $gccVersion" -ForegroundColor Gray
} else {
    Write-Host "WARNING: PowerISA cross-compiler not found" -ForegroundColor Yellow
    Write-Host "  Run toolchain/install-toolchain.ps1 to install" -ForegroundColor Yellow
}

Write-Host "`nToolchain setup check complete!" -ForegroundColor Green
Write-Host "Run 'make toolchain' to install missing components" -ForegroundColor Cyan


# AstraLisp OS PowerISA Cross-Compilation Toolchain Installer for Windows
# PowerShell script for Windows 11 Pro native installation

param(
    [string]$ToolchainDir = "$env:USERPROFILE\opt\powerpc64le-linux-gnu",
    [switch]$SkipClang = $false
)

$ErrorActionPreference = "Stop"

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "AstraLisp OS PowerISA Toolchain Installer" -ForegroundColor Cyan
Write-Host "Windows 11 Pro Native Installation" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""

# Check for WSL2
$wslAvailable = Get-Command wsl -ErrorAction SilentlyContinue
if (-not $wslAvailable) {
    Write-Host "WARNING: WSL2 not detected. For best results, use WSL2." -ForegroundColor Yellow
    Write-Host "Alternatively, use the WSL2 installer script instead." -ForegroundColor Yellow
    Write-Host ""
}

# Check for required tools
function Test-Dependencies {
    $missing = @()
    
    $required = @("wget", "tar", "make")
    foreach ($cmd in $required) {
        if (-not (Get-Command $cmd -ErrorAction SilentlyContinue)) {
            $missing += $cmd
        }
    }
    
    if ($missing.Count -gt 0) {
        Write-Host "Missing dependencies: $($missing -join ', ')" -ForegroundColor Red
        Write-Host "Please install them via chocolatey or manually." -ForegroundColor Red
        exit 1
    }
}

# Download and extract function
function Get-Archive {
    param(
        [string]$Url,
        [string]$OutputDir,
        [string]$ArchiveName
    )
    
    $archivePath = Join-Path $OutputDir $ArchiveName
    
    if (-not (Test-Path $archivePath)) {
        Write-Host "Downloading $ArchiveName..." -ForegroundColor Green
        wget -Uri $Url -OutFile $archivePath
    }
    
    $extractedDir = Join-Path $OutputDir ($ArchiveName -replace '\.(tar\.xz|tar\.gz|zip)$', '')
    if (-not (Test-Path $extractedDir)) {
        Write-Host "Extracting $ArchiveName..." -ForegroundColor Green
        tar -xf $archivePath -C $OutputDir
    }
    
    return $extractedDir
}

# Install via WSL2 (recommended)
function Install-ViaWSL {
    Write-Host "Installing via WSL2 (recommended method)..." -ForegroundColor Green
    Write-Host ""
    
    $scriptPath = Join-Path $PSScriptRoot "install-powerisa.sh"
    
    if (Test-Path $scriptPath) {
        wsl bash $scriptPath
    } else {
        Write-Host "WSL installer script not found. Please use WSL2 directly." -ForegroundColor Red
        exit 1
    }
}

# Main installation
function Main {
    Test-Dependencies
    
    Write-Host "Toolchain will be installed to: $ToolchainDir" -ForegroundColor Yellow
    Write-Host "Press Enter to continue or Ctrl+C to cancel..."
    Read-Host
    
    if ($wslAvailable) {
        Install-ViaWSL
    } else {
        Write-Host ""
        Write-Host "For Windows native installation, please use WSL2." -ForegroundColor Yellow
        Write-Host "Run this script from WSL2, or install WSL2 first." -ForegroundColor Yellow
        Write-Host ""
        Write-Host "To install WSL2:" -ForegroundColor Cyan
        Write-Host "  wsl --install" -ForegroundColor White
        exit 1
    }
    
    Write-Host ""
    Write-Host "==========================================" -ForegroundColor Cyan
    Write-Host "Toolchain installation complete!" -ForegroundColor Green
    Write-Host "==========================================" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "To use the toolchain from WSL2:" -ForegroundColor Yellow
    Write-Host "  source ~/opt/powerpc64le-linux-gnu/env.sh" -ForegroundColor White
    Write-Host ""
}

Main

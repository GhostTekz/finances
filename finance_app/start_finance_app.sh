#!/bin/bash
# start_finance_app.sh — Linux launch script for R Personal Finance Platform

set -e

APP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PORT=7777
LOG_FILE="$APP_DIR/finance_app.log"

# User-local R library — avoids needing root for package installs
R_LIBS_USER="${HOME}/.local/lib/R/library"
mkdir -p "$R_LIBS_USER"
export R_LIBS_USER

echo "============================================"
echo "  R Personal Finance Platform"
echo "============================================"

# ── Detect package manager and install R + system deps if missing ────────────
detect_pkg_manager() {
  if command -v dnf  &>/dev/null; then echo "dnf"
  elif command -v yum &>/dev/null; then echo "yum"
  elif command -v apt-get &>/dev/null; then echo "apt"
  elif command -v pacman &>/dev/null; then echo "pacman"
  elif command -v zypper &>/dev/null; then echo "zypper"
  else echo "unknown"
  fi
}

PKG_MGR=$(detect_pkg_manager)

if ! command -v Rscript &>/dev/null; then
  echo "R is not installed. Installing R..."
  case "$PKG_MGR" in
    dnf)
      sudo dnf install -y R R-devel gcc-gfortran \
        openssl-devel libcurl-devel libxml2-devel \
        harfbuzz-devel fribidi-devel freetype-devel libpng-devel libtiff-devel \
        libjpeg-turbo-devel pandoc fontconfig-devel cmake
      ;;
    apt)
      sudo apt-get update -qq
      sudo apt-get install -y r-base r-base-dev libssl-dev libcurl4-openssl-dev \
        libxml2-dev pandoc libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
        libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
      ;;
    pacman)
      sudo pacman -Sy --noconfirm r pandoc openssl libxml2 curl
      ;;
    zypper)
      sudo zypper install -y R-base R-base-devel libopenssl-devel libcurl-devel \
        libxml2-devel pandoc
      ;;
    *)
      echo "ERROR: Cannot auto-install R. Please install R manually from https://cran.r-project.org"
      exit 1
      ;;
  esac
fi

R_VERSION=$(Rscript --version 2>&1 | head -1)
echo "Using: $R_VERSION"
echo "R user library: $R_LIBS_USER"

# ── Install system libraries needed to compile R packages ────────────────────
echo "Checking system library dependencies..."
case "$PKG_MGR" in
  dnf|yum)
    sudo "$PKG_MGR" install -y \
      R-devel gcc-gfortran \
      openssl-devel libcurl-devel libxml2-devel \
      harfbuzz-devel fribidi-devel freetype-devel \
      libpng-devel libtiff-devel libjpeg-turbo-devel \
      fontconfig-devel pandoc cmake 2>/dev/null || true
    ;;
  apt)
    sudo apt-get install -y --no-install-recommends \
      libssl-dev libcurl4-openssl-dev libxml2-dev pandoc \
      libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
      libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev 2>/dev/null || true
    ;;
esac

# ── Install R packages into user library ─────────────────────────────────────
echo "Checking R package dependencies..."
R_LIBS_USER="$R_LIBS_USER" Rscript "$APP_DIR/install_packages.R"

# ── Create data directory ─────────────────────────────────────────────────────
mkdir -p "$APP_DIR/data"

# ── Kill any existing process on the port ────────────────────────────────────
if command -v lsof &>/dev/null && lsof -Pi :$PORT -sTCP:LISTEN -t &>/dev/null 2>&1; then
  echo "Port $PORT is already in use. Killing existing process..."
  lsof -ti :$PORT | xargs kill -9 2>/dev/null || true
  sleep 1
elif command -v ss &>/dev/null && ss -tlnp | grep -q ":$PORT "; then
  echo "Port $PORT in use — attempting to free it..."
  fuser -k ${PORT}/tcp 2>/dev/null || true
  sleep 1
fi

echo ""
echo "Starting Finance App at http://localhost:$PORT"
echo "R user library: $R_LIBS_USER"
echo "Log: $LOG_FILE"
echo "Press Ctrl+C to stop."
echo ""

# ── Open browser after a short delay ─────────────────────────────────────────
(sleep 3 && xdg-open "http://localhost:$PORT" 2>/dev/null || \
  open "http://localhost:$PORT" 2>/dev/null || true) &

# ── Launch the Shiny app ──────────────────────────────────────────────────────
cd "$APP_DIR"
R_LIBS_USER="$R_LIBS_USER" \
  Rscript -e "shiny::runApp('.', port=$PORT, host='127.0.0.1', launch.browser=FALSE)" \
  2>&1 | tee "$LOG_FILE"

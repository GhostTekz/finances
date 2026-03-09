#!/bin/bash
# start_finance_app.sh — Linux launch script for R Personal Finance Platform

set -e

APP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PORT=7777
LOG_FILE="$APP_DIR/finance_app.log"

echo "============================================"
echo "  R Personal Finance Platform"
echo "============================================"

# Check for R
if ! command -v Rscript &>/dev/null; then
  echo "R is not installed. Installing R..."
  sudo apt-get update -qq
  sudo apt-get install -y r-base r-base-dev libssl-dev libcurl4-openssl-dev \
    libxml2-dev libsodium-dev pandoc
fi

R_VERSION=$(Rscript --version 2>&1 | head -1)
echo "Using: $R_VERSION"

# Install system dependencies for R packages
if command -v apt-get &>/dev/null; then
  echo "Ensuring system libraries are present..."
  sudo apt-get install -y --no-install-recommends \
    libssl-dev libcurl4-openssl-dev libxml2-dev libsodium-dev pandoc \
    libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev 2>/dev/null || true
fi

# Install R packages
echo "Checking R package dependencies..."
Rscript "$APP_DIR/install_packages.R"

# Create data directory
mkdir -p "$APP_DIR/data"

# Check if port is already in use
if lsof -Pi :$PORT -sTCP:LISTEN -t &>/dev/null 2>&1; then
  echo "Port $PORT is already in use. Killing existing process..."
  lsof -ti :$PORT | xargs kill -9 2>/dev/null || true
  sleep 1
fi

echo ""
echo "Starting Finance App at http://localhost:$PORT"
echo "Log: $LOG_FILE"
echo "Press Ctrl+C to stop."
echo ""

# Open browser after a short delay
(sleep 3 && xdg-open "http://localhost:$PORT" 2>/dev/null || \
  open "http://localhost:$PORT" 2>/dev/null || true) &

# Launch the Shiny app
cd "$APP_DIR"
Rscript -e "shiny::runApp('.', port=$PORT, host='127.0.0.1', launch.browser=FALSE)" 2>&1 | tee "$LOG_FILE"

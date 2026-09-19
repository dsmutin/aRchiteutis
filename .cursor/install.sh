#!/usr/bin/env bash
## Cloud Agent environment bootstrap for aRchiteutis / samovar.
## Installs R, the system libraries the R packages link against, and all
## required R packages. Idempotent: safe to run repeatedly.
set -euo pipefail

export DEBIAN_FRONTEND=noninteractive

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "==> Installing system packages (R + runtime/build libraries)"
sudo apt-get update -qq
sudo apt-get install -y --no-install-recommends \
  r-base-core r-base-dev r-recommended \
  libcurl4-openssl-dev libssl-dev libxml2-dev \
  libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
  libpng-dev libtiff5-dev libjpeg-dev libgit2-dev libglpk-dev \
  cmake pkg-config

echo "==> Installing R packages"
## Install into the system site-library (writable via sudo) so packages are
## available to every user/process on the machine.
sudo Rscript "${SCRIPT_DIR}/install_packages.R"

echo "==> Environment bootstrap complete"
R --version | head -1

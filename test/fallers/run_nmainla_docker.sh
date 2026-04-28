#!/bin/bash
# Build a Linux container with INLA + nmaINLA and run nma_inla on the
# fallers arm-level reconstruction. Times the fit and writes a JSON.
set -euo pipefail

DOCKER_IMG=slmm-inla:latest
SLMM=/Users/tosku/Sync/Documents/slmm

# Use unlhcc/r-inla as base — has INLA pre-installed, just add nmaINLA + jsonlite.
docker build --platform linux/amd64 -t "$DOCKER_IMG" - <<'DOCKERFILE'
FROM unlhcc/r-inla:latest
RUN R -e 'options(repos = c(CRAN = "https://cloud.r-project.org")); \
          install.packages(c("jsonlite","dplyr","tidyr","nmaINLA"))' || true
DOCKERFILE

# Run the fit script inside the container
docker run --rm --platform linux/amd64 \
    -v "$SLMM:/slmm" -w /slmm "$DOCKER_IMG" \
    Rscript test/fallers/run_nmainla.R

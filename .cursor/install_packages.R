#!/usr/bin/env Rscript
## Idempotent installation of the R packages required by aRchiteutis / samovar.
## Uses the Posit Public Package Manager (P3M) binary repository for Ubuntu
## 24.04 (noble) so packages install as precompiled binaries (no compilation).

options(
  repos = c(P3M = "https://packagemanager.posit.co/cran/__linux__/noble/latest"),
  ## The User-Agent lets P3M serve Linux binaries for this R version/platform.
  HTTPUserAgent = sprintf(
    "R/%s R (%s)",
    getRversion(),
    paste(getRversion(), R.version$platform, R.version$arch, R.version$os)
  ),
  Ncpus = max(1L, parallel::detectCores())
)

pkgs <- c(
  ## core libraries declared in source_functions.R
  "tidyverse", "ggrepel", "corrplot", "viridis", "Rtsne",
  "circlize", "factoextra", "ggraph", "igraph",
  ## additional packages used by the plot/diversity functions
  "abdiv",      # alpha/beta diversity metrics + bray_curtis
  "usedist",    # dist_make() used by df2beta
  "heatmap3",   # df2beta heatmap
  "ape",        # pcoa() used by df2beta_pcoa
  "ggnewscale", # new_scale_colour() used by df2beta_pcoa
  "ggforce",    # geom_mark_ellipse() used by df2beta_pcoa
  "tsne"        # df2tsne
)

installed <- rownames(installed.packages())
to_install <- setdiff(pkgs, installed)

if (length(to_install) == 0) {
  cat("All required R packages already installed.\n")
} else {
  cat("Installing:", paste(to_install, collapse = ", "), "\n")
  install.packages(to_install)
}

missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing) > 0) {
  stop("Failed to install R packages: ", paste(missing, collapse = ", "))
}
cat("R package check complete: all", length(pkgs), "packages present.\n")

#!/usr/bin/env Rscript
## Idempotent installation of the R packages required by aRchiteutis / samovar.
## Uses the Posit Public Package Manager (P3M) binary repository for Ubuntu
## 24.04 (noble) so packages install as precompiled binaries (no compilation).

options(
  repos = c(
    P3M = "https://packagemanager.posit.co/cran/__linux__/noble/latest",
    ## Bioconductor 3.18 (matches R 4.3) via the Posit Public Package Manager so
    ## Bioconductor deps such as phyloseq resolve from the same mirror.
    BioCsoft = "https://packagemanager.posit.co/bioconductor/packages/3.18/bioc",
    BioCann = "https://packagemanager.posit.co/bioconductor/packages/3.18/data/annotation",
    BioCexp = "https://packagemanager.posit.co/bioconductor/packages/3.18/data/experiment"
  ),
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
  "tsne",       # df2tsne
  ## Bioconductor: phyloseq interoperability (ps2df / df2ps)
  "phyloseq"
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

## ggviolinbox: soft (Suggests) dependency for the optional violinbox rendering
## in df2alpha() / df2alpha_summary() / df2barplot(). Installed from GitHub
## (MIT, github.com/dsmutin/ggviolinbox), idempotently. Its DESCRIPTION declares
## R (>= 4.4); we relax the remotes warning-to-error so it installs on R 4.3.
if (!requireNamespace("ggviolinbox", quietly = TRUE)) {
  Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("dsmutin/ggviolinbox", upgrade = "never")
  if (!requireNamespace("ggviolinbox", quietly = TRUE)) {
    stop("Failed to install ggviolinbox from GitHub (dsmutin/ggviolinbox).")
  }
  cat("Installed ggviolinbox from GitHub.\n")
} else {
  cat("ggviolinbox already installed.\n")
}

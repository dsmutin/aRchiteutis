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
  ## tidygraph -> ggraph engine (df2graph / df2ggraph)
  "tidygraph",
  ## Vignette toolchain (Suggests): knitr + rmarkdown build the HTML usage
  ## vignettes, ggpubr styles the statistical plots (ggviolin/ggboxplot +
  ## stat_compare_means) and vegan::adonis2() runs the PERMANOVA beta-diversity
  ## group test. (pandoc, required by rmarkdown, is installed via apt below.)
  "knitr", "rmarkdown", "ggpubr", "vegan",
  ## additional packages used by the plot/diversity functions
  "abdiv",      # alpha/beta diversity metrics + bray_curtis
  "usedist",    # dist_make() used by df2beta
  "heatmap3",   # df2beta heatmap
  "ape",        # pcoa() used by df2beta_pcoa
  "ggnewscale", # new_scale_colour() used by df2beta_pcoa
  "ggforce",    # geom_mark_ellipse() used by df2beta_pcoa
  "tsne",       # df2tsne
  ## Bioconductor: phyloseq interoperability (ps2df / df2ps)
  "phyloseq",
  ## Optional beta-diversity backends (Suggests): compositional / Aitchison
  ## distance (robCompositions::aDist + zCompositions::cmultRepl for zero
  ## replacement) and the adiv Jaccard family (adiv::Jac). Both are CRAN
  ## binaries. `adiv` Imports rgl (needs OpenGL) and phytools; loading it on a
  ## headless box requires RGL_USE_NULL=TRUE (set in tests and in the guarded
  ## runtime code) plus the GL system libraries installed below.
  "robCompositions", "zCompositions", "adiv"
)

## System GL libraries required by rgl (a transitive dependency of adiv). On a
## headless machine rgl only loads with RGL_USE_NULL=TRUE, but the shared
## libraries must still be present. Installed idempotently; failures are
## non-fatal (apt may be unavailable in some environments).
## `pandoc` is additionally required by rmarkdown to knit the HTML vignettes.
if (Sys.info()[["sysname"]] == "Linux" && nzchar(Sys.which("apt-get"))) {
  sys_libs <- c("libgl1-mesa-dev", "libglu1-mesa-dev", "libx11-dev", "pandoc")
  try(system(paste(
    "sudo apt-get install -y --no-install-recommends",
    paste(sys_libs, collapse = " ")), ignore.stdout = TRUE,
    ignore.stderr = TRUE), silent = TRUE)
}

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

## NetCoMi: optional (Suggests) microbial-network support for df2netcomi() /
## df2netcomi_graph(). Installed from GitHub (github.com/stefpeschel/NetCoMi).
## Wrapped in tryCatch so a NetCoMi failure NEVER breaks the bootstrap: the
## package's NetCoMi code is guarded by requireNamespace() and simply errors
## with an install hint when NetCoMi is absent.
##
## This environment runs R 4.3.3 / Bioconductor 3.18, but NetCoMi HEAD and its
## SPRING/SpiecEasi HEAD dependencies now require R >= 4.5/4.6, so we pin
## R-4.3-compatible refs: NetCoMi v1.1.0, SpiecEasi v1.1.1, and the SPRING
## commit (3d641a4) that still targets R >= 2.10. SpiecEasi links against a
## modern RcppArmadillo that needs C++14, so we force C++14 for C++11 packages.
if (!requireNamespace("NetCoMi", quietly = TRUE)) {
  ok <- tryCatch({
    Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")

    ## Force C++14 for packages that declare only C++11 (SpiecEasi + modern
    ## RcppArmadillo). Written to the running user's ~/.R/Makevars.
    mk_dir <- path.expand("~/.R")
    if (!dir.exists(mk_dir)) dir.create(mk_dir, recursive = TRUE)
    mk <- file.path(mk_dir, "Makevars")
    mk_lines <- if (file.exists(mk)) readLines(mk) else character(0)
    for (v in c("CXX11STD = -std=gnu++14", "CXX14STD = -std=gnu++14")) {
      key <- sub(" .*$", "", v)
      mk_lines <- mk_lines[!grepl(paste0("^", key, "\\b"), mk_lines)]
      mk_lines <- c(mk_lines, v)
    }
    writeLines(mk_lines, mk)

    if (!requireNamespace("remotes", quietly = TRUE))
      install.packages("remotes")

    ## pulsar was archived on CRAN (2026): install its last source version.
    if (!requireNamespace("pulsar", quietly = TRUE)) {
      install.packages(paste0("https://cran.r-project.org/src/contrib/",
                              "Archive/pulsar/pulsar_0.3.13.tar.gz"),
                       repos = NULL, type = "source")
    }

    ## Binary CRAN/Bioc dependencies (incl. limma, used by netConstruct()).
    nc_deps <- c("mixedCCA", "huge", "rootSolve", "mvtnorm", "VGAM",
                 "corrplot", "doSNOW", "foreach", "fdrtool", "filematrix",
                 "gtools", "orca", "qgraph", "RColorBrewer", "Rdpack",
                 "vegan", "WGCNA", "Biobase", "limma")
    nc_deps <- setdiff(nc_deps, rownames(installed.packages()))
    if (length(nc_deps) > 0) install.packages(nc_deps)

    if (!requireNamespace("SpiecEasi", quietly = TRUE))
      remotes::install_github("zdk123/SpiecEasi@v1.1.1", upgrade = "never",
                              dependencies = c("Depends", "Imports",
                                               "LinkingTo"))
    if (!requireNamespace("SPRING", quietly = TRUE))
      remotes::install_github("GraceYoon/SPRING@3d641a4", upgrade = "never",
                              dependencies = c("Depends", "Imports",
                                               "LinkingTo"))
    remotes::install_github("stefpeschel/NetCoMi@v1.1.0",
                            dependencies = c("Depends", "Imports", "LinkingTo"),
                            upgrade = "never")
    requireNamespace("NetCoMi", quietly = TRUE)
  }, error = function(e) {
    cat("WARNING: NetCoMi install failed (non-fatal):", conditionMessage(e),
        "\n")
    FALSE
  })
  if (isTRUE(ok)) {
    cat("Installed NetCoMi from GitHub (v1.1.0).\n")
  } else {
    cat("NetCoMi not installed; df2netcomi()/df2netcomi_graph() will error ",
        "with an install hint until it is available.\n", sep = "")
  }
} else {
  cat("NetCoMi already installed.\n")
}

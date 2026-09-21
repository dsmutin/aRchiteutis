# aRchiteutis <img src="img/logo.png" align="right" width="220" alt="aRchiteutis">

[![R-CMD-check](https://github.com/dsmutin/aRchiteutis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dsmutin/aRchiteutis/actions/workflows/R-CMD-check.yaml)
[![R-package](https://img.shields.io/badge/R-package-276DC3?logo=r&logoColor=white)](https://github.com/dsmutin/aRchiteutis)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

R package for Kraken2, Kaiju and Bracken reports: read them into a tidy table, or start from an abundance matrix, a phyloseq object, or a table exported from Python, then plot composition, diversity and ordination.

## Install

```r
# install.packages("remotes")
remotes::install_github("dsmutin/aRchiteutis")
library(aRchiteutis)
```

## Load data

Kraken2 reports (example files ship in the package):

```r
path   <- system.file("extdata", package = "aRchiteutis")
legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
df <- get_counts(path, pattern = "decont_b", legend = legend, trim_char = "_")
```

Other inputs, same plotting functions:

```r
df <- from_abundance(count_matrix, clade = "G")   # taxa in rows, samples in columns
df <- from_phyloseq(physeq, taxa_rank = "Genus")
df <- python2r("abundance.csv", clade = "G")      # CSV from inst/python/python2r.py
```

The table is long: `taxa`, `clade`, `sample`, `N`, `amount`, `amount_cl`, plus legend columns. `df_untidy()` turns it into a taxa-by-sample matrix. Trim with `df_taxa_trim()` or `df_get_top_taxa()` before plotting. Argument details and a second copy of every figure are in [vignettes.md](vignettes.md).

## Plots

Figures below are rendered from the bundled honey-bee brood reports.

### Composition

`df2donut()` — mean composition.

<img src="img/donut.png" alt="Donut of mean composition" width="720">

`df2composition()` — stacked bars per sample.

<img src="img/composition.png" alt="Stacked composition bars" width="720">

`df2barplot()` — amount per taxon.

<img src="img/barplot.png" alt="Box plot of taxon amounts" width="720">

### Diversity

`df2alpha_summary()` — alpha-diversity indices. `split_by` is a legend column (here, brood stage).

<img src="img/alpha.png" alt="Alpha-diversity summary" width="720">

`df2beta()` — Bray–Curtis distances between samples. Point colour is brood stage.

<img src="img/beta.png" alt="Beta-diversity heatmap" width="720">

`df2beta_pcoa()` — PCoA of that distance, with group ellipses.

<img src="img/beta_pca.png" alt="Beta-diversity PCoA" width="720">

### Clustering and correlation

`df2cluster()` — dendrogram of taxa or of samples. Leaf colour is the cluster.

<img src="img/cluster1.png" alt="Taxon dendrogram" width="480"><img src="img/cluster2.png" alt="Sample dendrogram" width="420">

`df2clust2d()` — mean abundance in two groups on a log10 scale (pupa vs larvae).

<img src="img/cluster2d.png" alt="Two-group cluster scatter" width="720">

`df2heatmap()` — taxa by sample.

<img src="img/heatmap.png" alt="Taxon heatmap" width="720">

`df2corrplot()` — correlations between taxa.

<img src="img/corr.png" alt="Taxon correlation plot" width="640">

### Ordination and group differences

`df2chord()` — circular co-occurrence graph.

<img src="img/chord.png" alt="Chord plot of taxon correlations" width="640">

`df2tsne()` — t-SNE of taxa.

<img src="img/tsne.png" alt="t-SNE of taxa" width="720">

`df2volcano()` — log abundance change between two legend groups.

<img src="img/volcano.png" alt="Volcano plot" width="720">

`df2pca_sample()` — PCA of samples. `df2pca_sp()` — PCA of taxa.

<img src="img/pca1.png" alt="PCA of samples" width="480"><img src="img/pca2.png" alt="PCA of taxa" width="480">

## Cite

```r
citation("aRchiteutis")
```

The record lives in [`inst/CITATION`](inst/CITATION): Smutin, Taldaev, Lebedev & Adonin, *International Journal of Molecular Sciences* 25(2):741, 2024. <https://doi.org/10.3390/ijms25020741>

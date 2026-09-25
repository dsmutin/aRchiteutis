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

## Report

`archi_report()` reads a directory of Kraken, Kaiju or QIIME 2 profiles (or a taxid abundance table), builds a phyloseq object with a taxonomy tree, and drops samples that fall below a read floor or whose rarefaction curve is still rising. Each requested plot is written with a one-line method caption. The HTML and the MultiQC section both end by stating that this is a preliminary report, not a final analysis.

```r
path   <- system.file("extdata", package = "aRchiteutis")
legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
archi_report(
  path, legend, outdir = "report",
  pattern = "_k2\\.txt$", trim_char = "_", target = "stage",
  plots = c("composition", "donut", "alpha", "beta", "rarefaction")
)
```

`source` is `"auto"`, `"kraken"`, `"kaiju"`, `"qza"` or `"abundance"`. `plots` may also include `barplot`, `heattree`, `upset` and `difftree`. `beta_method` is any name from `archi_beta_methods()`.

A rendered PDF of that report on the bundled Kraken2 profiles is [`examples/kraken2-report.pdf`](examples/kraken2-report.pdf).

## Skills

Agent skills in [`skills/`](skills/) only call package functions. Start with [`skills/aRchiteutis/SKILL.md`](skills/aRchiteutis/SKILL.md).

| Step | Skills |
| --- | --- |
| Load a tidy table | `aRchiteutis-load-kraken`, `aRchiteutis-load-abundance`, `aRchiteutis-load-phyloseq`, `aRchiteutis-load-python` |
| Import to phyloseq | `aRchiteutis-import-kraken`, `aRchiteutis-import-kaiju`, `aRchiteutis-import-qza`, `aRchiteutis-import-abundance`, `aRchiteutis-taxonomy-tree` |
| Trim and reshape | `aRchiteutis-transform` |
| Plot | `aRchiteutis-plot-composition`, `aRchiteutis-plot-diversity`, `aRchiteutis-plot-rarefaction`, `aRchiteutis-plot-difftree`, `aRchiteutis-plot-heattree`, `aRchiteutis-plot-ordination` |
| Preliminary report | `aRchiteutis-report` |

The same files are mirrored under `.cursor/skills/` for the editor.

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

`df2composition()` — stacked bars per sample. Samples are ordered by the first principal component (`order_samples = "fpc"`). `hclust`, `abundance`, `alpha` and `none` are the other orders.

<img src="img/composition.png" alt="Stacked composition bars" width="720">

`df2barplot()` — amount per taxon. `style = "raincloud"` draws a ggviolinbox raincloud.

`df2composition_tree()` — fan cladogram from [ggtree](https://doi.org/10.1111/2041-210X.12628) (Yu et al. 2017) with a [ggtreeExtra](https://doi.org/10.1093/molbev/msab166) boxplot of relative abundance (Xu et al. 2021). Tips and boxes are coloured by phylum when a rank table is available.

<img src="img/composition_tree.png" alt="Fan tree of composition" width="720">

<img src="img/barplot_raincloud.png" alt="Raincloud of taxon amounts" width="720">

### Diversity

`df2alpha_summary()` — alpha-diversity indices. `split_by` is a legend column (here, brood stage).

<img src="img/alpha.png" alt="Alpha-diversity summary" width="720">

`df2alpha()` — Shannon and Simpson. `style = "raincloud"` uses the same raincloud geometry.

<img src="img/alpha_raincloud.png" alt="Raincloud of Shannon and Simpson" width="720">

`df2rarefaction()` — observed richness, Shannon and Simpson against sequencing depth. One thin line per sample, a smooth by group.

<img src="img/rarefaction.png" alt="Alpha rarefaction curves" width="720">

`df2beta()` — Bray–Curtis distances between samples. Point colour is brood stage. `method` accepts any name from `archi_beta_methods()`, including `"aitchison"`.

<img src="img/beta.png" alt="Beta-diversity heatmap" width="720">

`df2beta_pcoa()` — PCoA of that distance, with group ellipses.

<img src="img/beta_pca.png" alt="Beta-diversity PCoA" width="720">

### Taxonomy

`df2heattree()` — metacoder heat tree. Node size is the number of taxa, colour is mean relative abundance. Empty ranks are dropped and their children reattached, so the taxonomy is one tree. The function stops when \pkg{metacoder} is not installed.

<img src="img/heattree.png" alt="Taxonomic heat tree" width="720">

`df2difftree()` — log2 fold change between two groups on a ggtree layout. The default fruit is a heatmap of relative abundance; tip colour is the fold change. `fruit = "bar"` draws the fold change as a column. With more than two groups, pass `contrast`. `engine = "metacoder"` draws `metacoder::heat_tree` and stops when that package is not installed. `engine = "microbiota"` runs MicrobiotaProcess `mp_diff_analysis` and stops when that package is not installed.

<img src="img/difftree.png" alt="Differential abundance tree" width="720">

`engine = "microbiota"` draws the MicrobiotaProcess biomarker tree (radial ggtree, phylum highlight, abundance stars, LDA and FDR) when that package is installed.

<img src="img/difftree_microbiota.png" alt="MicrobiotaProcess differential tree" width="720">

`df2upset()` — taxon presence as a ComplexUpset plot. The function stops when \pkg{ComplexUpset} is not installed.

<img src="img/upset.png" alt="UpSet of taxon presence" width="720">

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

`df2volcano()` — ANCOM-BC2 log2 fold change against adjusted q. The first `legend_detect` pattern is the reference group. The function stops when \pkg{ANCOMBC} is not installed.

<img src="img/volcano.png" alt="Volcano plot" width="720">

`df2pca_sample()` — PCA of samples. `df2pca_sp()` — PCA of taxa.

<img src="img/pca1.png" alt="PCA of samples" width="480"><img src="img/pca2.png" alt="PCA of taxa" width="480">

## Cite

```r
citation("aRchiteutis")
```

The record lives in [`inst/CITATION`](inst/CITATION): Smutin, Taldaev, Lebedev & Adonin, *International Journal of Molecular Sciences* 25(2):741, 2024. <https://doi.org/10.3390/ijms25020741>

---
name: aRchiteutis
description: Run the full aRchiteutis metagenomics visualization pipeline from Kraken2 reports, a taxa-by-sample abundance table, a phyloseq object, or a Python-loaded dataset through tidy transforms into df2* plots. Use when the user wants the complete path (load → trim/untidy → composition/diversity/ordination) in R package aRchiteutis.
---

# aRchiteutis — full pipeline

Install / attach:

```r
# remotes::install_github("dsmutin/aRchiteutis")
library(aRchiteutis)
```

Then pick **one** loader, transform, and plot. Do not invent new column names; every plot expects the canonical tidy table (`taxa`, `clade`, `sample`, `N`, `amount`, `amount_cl`, plus legend columns) or a taxa-by-sample matrix from `df_untidy()`.

## 1. Load (exactly one)

| Source | Call | Skill |
| --- | --- | --- |
| Kraken2 / Kaiju / Bracken reports | `get_counts(path, pattern, legend, trim_char, output_type)` | `aRchiteutis-load-kraken` |
| Kraken / Kraken2 / KrakenUniq / Bracken → phyloseq | `kraken_to_phyloseq(path, pattern, rank, legend, trim_char)` | `aRchiteutis-import-kraken` |
| Kaiju → phyloseq | `kaiju_to_phyloseq(path, pattern, legend, trim_char)` | `aRchiteutis-import-kaiju` |
| QIIME 2 `.qza` artifacts → phyloseq | `qza_to_phyloseq(features, taxonomy, metadata, tree)` | `aRchiteutis-import-qza` |
| Abundance table with NCBI taxids → phyloseq | `abundance_taxid_to_phyloseq(counts, metadata, xml)` | `aRchiteutis-import-abundance` |
| Abundance matrix / CSV | `from_abundance(x, clade, legend)` | `aRchiteutis-load-abundance` |
| phyloseq object or unpacked list | `from_phyloseq(physeq, taxa_rank)` | `aRchiteutis-load-phyloseq` |
| Taxids or a rank table → taxonomy tree | `taxids_to_tree()` / `ranks_to_tree()` | `aRchiteutis-taxonomy-tree` |
| Python-loaded table | dump with `inst/python/python2r.py`, then `python2r(csv, clade)` | `aRchiteutis-load-python` |

Example Kraken2 (bundled data):

```r
path <- system.file("extdata", package = "aRchiteutis")
legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
df <- get_counts(path = path, pattern = "decont_b", legend = legend, trim_char = "_")
```

## 2. Transform

See `aRchiteutis-transform`. Typical sequence:

```r
df_nosp <- df[df$clade != "S", ]
dfT <- df_taxa_trim(df_nosp, top_taxa = 15)
dfC <- df_get_top_taxa(df, clade = "G", top = 50)
matG <- df_untidy(df, clade = "G", top = 30, scale = "scale")
```

## 3. Visualize

- Composition (`df2donut`, `df2composition`, `df2barplot`) → `aRchiteutis-plot-composition`. `df2composition(..., order_samples = "fpc")` is the default sample order; `hclust`, `abundance`, `alpha` and `none` are the other choices. `df2barplot(..., style = "raincloud")` uses ggviolinbox.
- Diversity (`df2alpha_summary`, `df2alpha`, `df2beta`, `df2beta_pcoa`) → `aRchiteutis-plot-diversity`. `style = "raincloud"` on the alpha plots. `df2beta(..., method = "aitchison")` and the other names from `archi_beta_methods()`.
- Rarefaction, differential tree, heat tree, UpSet (`df2rarefaction`, `df2difftree`, `df2heattree`, `df2upset`) → `aRchiteutis-plot-rarefaction`, `aRchiteutis-plot-difftree`, `aRchiteutis-plot-heattree`.
- Ordination / networks (`df2heatmap`, `df2cluster`, `df2clust2d`, `df2corrplot`, `df2chord`, `df2tsne`, `df2volcano`, `df2pca_sample`, `df2pca_sp`) → `aRchiteutis-plot-ordination`

## 4. Preliminary report

`archi_report()` is the only report entry point. See `aRchiteutis-report`. Pass `plots` and `beta_method` to rebuild the draft with a different subset. The HTML must keep the sentence that this is a preliminary report only.

Gallery with images: `vignettes.md`. Demo script: `examples/pipeline.R`.

## Rules

- Prefer `library(aRchiteutis)` over sourcing old `functions/` or `plots/` scripts (those were folded into `R/`).
- Keep species (`clade == "S"`) for alpha diversity and sample PCA; drop them for composition trims when the tree is too deep.
- Legend columns are joined after `sample`; `add_legend = 7` means the 7th column of the tidy table.
- For Python, never pass a live pandas object unless `reticulate` is installed; the CSV bridge is the default.

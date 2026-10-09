---
name: aRchiteutis-plot-difftree
description: Draw a differential-abundance tree with aRchiteutis df2difftree. Use for a ggtree abundance heatmap, a fold-change bar, a metacoder heat tree, or a MicrobiotaProcess biomarker tree.
---

# Differential tree

`df2difftree()` computes log2 fold change between two levels of a grouping column and draws the largest changes on a seven-rank taxonomy tree when `tax` is passed (`ranks_to_tree()`, not `hclust`). The default engine is ggtree: a circular cladogram (`layout = "rectangular"` is the other layout), one `geom_tiplab`, and a `ggtree::gheatmap` of relative abundance. Tip colour is the log2 fold change. `fruit = "bar"` draws that fold change as a column instead. `engine = "metacoder"` draws `metacoder::heat_tree` and stops when metacoder is not installed. `engine = "microbiota"` runs `MicrobiotaProcess::mp_diff_analysis` and draws the ggtree / ggtreeExtra biomarker tree (phylum highlight, abundance stars, LDA, FDR). It stops when MicrobiotaProcess is not installed. Pass `contrast` when the grouping column has more than two levels.

```r
library(aRchiteutis)
path <- system.file("extdata", package = "aRchiteutis")
legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
df <- get_counts(path, pattern = "decont_b", legend = legend, trim_char = "_")

df2difftree(df[df$clade == "S", ], group = "stage", max_tips = 20, tax = tax)
df2difftree(df[df$clade == "S", ], group = "stage", fruit = "bar",
            contrast = c("larvae", "pupa"), layout = "rectangular")
df2difftree(df[df$clade == "G", ], group = "stage", engine = "microbiota",
            contrast = c("larvae", "pupa"), tax = tax)
```

Pass `tree =` an `ape::phylo` whose tip labels match `taxa` when a taxonomy tree is already built with `ranks_to_tree()` or `taxids_to_tree()`.

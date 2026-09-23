---
name: aRchiteutis-plot-difftree
description: Draw a differential-abundance tree with aRchiteutis df2difftree. Use for ggtree fruit bars, log2 fold change on a taxonomy tree, or two-group taxon trees.
---

# Differential tree

`df2difftree()` computes log2 fold change between two levels of a grouping column and draws the largest changes on a genus/species formula tree (`ape::as.phylo`, not `hclust`). With `ggtree` and `ggtreeExtra` installed it uses the fruit-bar layout; otherwise it draws a rectangular cladogram.

```r
library(aRchiteutis)
path <- system.file("extdata", package = "aRchiteutis")
legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
df <- get_counts(path, pattern = "decont_b", legend = legend, trim_char = "_")

df2difftree(df[df$clade == "S", ], group = "stage", max_tips = 20)
df2difftree(df[df$clade == "S", ], group = "stage",
            contrast = c("larvae", "pupa"), layout = "rectangular")
```

Pass `tree =` an `ape::phylo` whose tip labels match `taxa` when a taxonomy tree is already built with `ranks_to_tree()` or `taxids_to_tree()`.

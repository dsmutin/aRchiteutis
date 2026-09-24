---
name: aRchiteutis-plot-difftree
description: Draw a differential-abundance tree with aRchiteutis df2difftree. Use for ggtree fruit bars, log2 fold change on a taxonomy tree, or two-group taxon trees.
---

# Differential tree

`df2difftree()` computes log2 fold change between two levels of a grouping column and draws the largest changes on a genus/species formula tree (`ape::as.phylo`, not `hclust`). With `ggtree` installed the tree uses that layout and a bar at each tip shows the log2 fold change. Otherwise it draws a rectangular cladogram with the same bar. Pass `contrast` when the grouping column has more than two levels.

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

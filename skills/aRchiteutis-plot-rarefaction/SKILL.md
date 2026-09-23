---
name: aRchiteutis-plot-rarefaction
description: Draw alpha-diversity rarefaction curves with aRchiteutis df2rarefaction. Use for rarefaction, sequencing-depth curves, Observed/Shannon/Simpson versus depth.
---

# Rarefaction

Use the tidy table from `get_counts()`, `from_abundance()`, `from_phyloseq()`, or `python2r()`. Keep species rows. Do not call a hook or a script outside the package.

```r
library(aRchiteutis)
path <- system.file("extdata", package = "aRchiteutis")
legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
df <- get_counts(path, pattern = "decont_b", legend = legend, trim_char = "_")

df2rarefaction(df[df$clade == "S", ], split_by = "stage", top = 40)
```

`split_by` is a legend column (name or index). `depths = NULL` uses the package depth grid. The replicate table is `attr(plot, "rarefaction")`.

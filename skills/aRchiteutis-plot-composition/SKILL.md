---
name: aRchiteutis-plot-composition
description: Draw aRchiteutis composition plots (donut, stacked bar, box plot) with df2donut, df2composition, and df2barplot from a tidy count table. Use when the user wants taxonomic composition figures.
---

# Composition plots

Need a **long** tidy table, usually trimmed:

```r
dfT <- df_taxa_trim(df[df$clade != "S", ], top_taxa = 15)
dfC <- df_get_top_taxa(df, clade = "G", top = 50)

df2donut(dfT)          # mean composition donut
df2composition(dfT)                     # samples ordered by FPC (PC1)
df2composition(dfT, order_samples = "hclust")  # or abundance, alpha, none
df2barplot(dfC)        # box plot per taxon
df2barplot(dfC, style = "raincloud")  # needs ggviolinbox
```

Images: `vignettes.md` (donut, composition, barplot). All three return ggplot objects.

If the user started from Python / phyloseq / a matrix, load first (`python2r`, `from_phyloseq`, `from_abundance`) then plot the same way.

---
name: aRchiteutis-plot-composition
description: Draw aRchiteutis composition plots (donut, stacked bar, box plot, fan tree) with df2donut, df2composition, df2barplot, and df2composition_tree from a tidy count table. Use when the user wants taxonomic composition figures.
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
df2composition_tree(dfC, top = 20)  # ggtree fan + ggtreeExtra abundance boxplots
```

`df2composition_tree()` follows the ggtree / ggtreeExtra fruit-boxplot layout (`layout = "fan"`, `open.angle = 10`, `ggtree::rotate_tree()`). Pass `tax` with a `phylum` column to colour tips by phylum; otherwise tips are coloured by genus.

Images: `vignettes.md` (donut, composition, barplot). All three return ggplot objects.

If the user started from Python / phyloseq / a matrix, load first (`python2r`, `from_phyloseq`, `from_abundance`) then plot the same way.

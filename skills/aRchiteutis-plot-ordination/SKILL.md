---
name: aRchiteutis-plot-ordination
description: Draw aRchiteutis heatmaps, clusters, correlation, chord, t-SNE, volcano, and PCA plots (df2heatmap, df2cluster, df2clust2d, df2corrplot, df2chord, df2tsne, df2volcano, df2pca_sample, df2pca_sp). Use for ordination, co-occurrence, and group-change figures.
---

# Ordination, clustering, networks

Most of these need a **taxa-by-sample matrix** from `df_untidy()`:

```r
matG <- df_untidy(df, clade = "G", top = 30, scale = "scale")

df2heatmap(df_untidy(df, clade = "G", top = 10), scale = "row", Colv = NA)
df2cluster(matG, k_means = 10, use = "sp")       # or use = "sample"
df2corrplot(matG, k_means = 5)
df2chord(matG, k_means = 10, coenf_level = 0.7)
df2tsne(matG, k_means = 10, text_top = 20)
df2pca_sp(df_untidy(df, clade = "G", top = 10), scale = TRUE)
```

Long-table plots (not untidy):

```r
df2clust2d(df[df$clade == "G", ], legend_detect = "pupa", top = 40, k_means = 10)
df2volcano(df[df$clade == "G", ], legend_detect = c("pupa", "larvae"))  # ANCOMBC::ancombc2
df2pca_sample(
  df_untidy(df, clade = "S", scale = "scale", keep_sample_name = FALSE),
  scale = FALSE, detect = "pupa")
```

Notes:

- `df2heatmap`, `df2cluster`, `df2corrplot`, `df2beta` draw on the current graphics device (base R / corrplot); wrap in `png()`/`pdf()` to save.
- `df2chord` / `df2tsne` / PCA / volcano / clust2d return ggplot / ggraph objects.
- `k_means < 2` is valid for `df2cluster` (no rectangles).
- For t-SNE on tiny matrices, pass `perplexity` and `max_iter` so it can finish.

Images: `vignettes.md`.

---
name: aRchiteutis-plot-diversity
description: Calculate and plot aRchiteutis alpha and beta diversity (df2alpha, df2alpha_summary, df2beta, df2beta_bray, df2beta_pcoa). Use when the user asks for Shannon/Simpson, Bray-Curtis, PCoA, or diversity heatmaps.
---

# Diversity plots

Use the **long** tidy table. Keep species for alpha; genus is typical for beta.

```r
sp <- df[df$clade == "S", ]
g  <- df[df$clade == "G", ]

df2alpha_summary(sp, split_by = 7, add_legend = 7:8)
df2alpha(sp, add_legend = 7)

df2beta(g, add_legend = 7:8, add_labels = 7)          # heatmap (side effect)
df2beta_bray(g, print_df = TRUE)                      # Bray-Curtis matrix
df2beta_pcoa(g, add_legend = 7, add_ellipse = 7)      # ggplot PCoA
```

`split_by` / `add_legend` / `add_ellipse` are **column numbers** of the tidy table (legend fields start at column 7 after `get_counts()` + CSV legend).

`df2beta(..., print_df = TRUE)` returns the distance matrix instead of drawing. Distances default to `abdiv::bray_curtis`; override with `dist_function`.

Images: `vignettes.md` (alpha, beta, beta_pca).

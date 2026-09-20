---
name: aRchiteutis-transform
description: Trim, rescale, drop unclassified taxa, add parent ranks, and pivot an aRchiteutis tidy table into a taxa-by-sample matrix with df_untidy() and related helpers. Use after loading data and before df2* plots that need a subset or a matrix.
---

# Transform the tidy table

Input is the canonical long table from `get_counts()` / `from_abundance()` / `from_phyloseq()` / `python2r()`.

| Function | What it does |
| --- | --- |
| `df_tidy_drop_unclassified(df)` | Drop taxa whose name contains `"unclassified"` |
| `df_remove_taxa(df, taxa, clade)` | Drop named taxa |
| `df_rescale(df)` | Recompute `amount` so it sums to 1 per sample |
| `df_drop_clade(df)` | Keep canonical ranks `U,R,D,P,C,O,F,G,S` |
| `df_get_parents(df)` | Add parent-taxon columns per rank |
| `df_taxa_trim(df, top_taxa)` | Keep a small set of lineages; roll rest into `"other <domain>"` |
| `df_get_top_taxa(df, clade, top)` | Filter long table to the top taxa at one rank |
| `df_untidy(df, clade, top, scale, amount_from, keep_sample_name)` | Taxa-by-sample **matrix** |
| `df_smart_bind(f1, ...)` | Row-bind tables, recomputing unclassified from `f1` |

Typical plot-ready objects:

```r
dfT  <- df_taxa_trim(df[df$clade != "S", ], top_taxa = 15)   # donut / stacked bar
dfC  <- df_get_top_taxa(df, clade = "G", top = 50)           # box plot
matG <- df_untidy(df, clade = "G", top = 30, scale = "scale") # heatmap, cluster, chord, t-SNE
```

`df_untidy()` arguments: `amount_from` is `"amount"` (default), `"N"`, or `"amount_cl"`; `scale` is `FALSE`, `"log2"`, or `"scale"`; `trim` is a legacy alias for `top`.

Keep `clade == "S"` rows for alpha diversity and sample PCA; they are noisy for composition trims.

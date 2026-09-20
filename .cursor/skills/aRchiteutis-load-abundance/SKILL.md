---
name: aRchiteutis-load-abundance
description: Convert a taxa-by-sample abundance matrix or long OTU table into the aRchiteutis tidy format with from_abundance() / as_archi(), then plot. Use when the user already has counts or relative abundances, not Kraken reports.
---

# Load an abundance table

Canonical target: long tibble with `taxa`, `clade`, `sample`, `N`, `amount`, `amount_cl`.

```r
library(aRchiteutis)

# Wide matrix: taxa in rows, samples in columns
df <- from_abundance(count_matrix, clade = "G", legend = sample_meta)

# Wide data frame whose first column is taxon names
df <- from_abundance(as.data.frame(wide_csv), clade = "S")

# Already long (aliases like otu/count/sample are mapped)
df <- as_archi(long_df, clade = "G")
```

`legend` is a data frame or CSV path with a `sample` column (or the first column as the sample key).

`clade` is the Kraken-style rank letter: `D` domain, `P` phylum, `C` class, `O` order, `F` family, `G` genus, `S` species.

Then plot, e.g. `df2composition(df)` or `df_untidy(df)` → heatmap/PCA. See `aRchiteutis-transform` and the plot skills.

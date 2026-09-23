---
name: aRchiteutis-plot-heattree
description: Draw a taxonomic heat tree with aRchiteutis df2heattree. Use for metacoder heat_tree or a rank tree coloured by abundance.
---

# Heat tree

`df2heattree()` colours a rank tree by mean relative abundance. If `metacoder` is installed and `tax` has Linnaean columns, it calls `metacoder::heat_tree`. Otherwise it draws a genus/species formula tree.

```r
library(aRchiteutis)
path <- system.file("extdata", package = "aRchiteutis")
df <- get_counts(path, pattern = "decont_b", trim_char = "_")
df2heattree(df[df$clade == "S", ], top = 20)
```

UpSet presence across a legend column is `df2upset(df, group = "stage")`.

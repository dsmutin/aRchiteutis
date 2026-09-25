---
name: aRchiteutis-plot-heattree
description: Draw a taxonomic heat tree with aRchiteutis df2heattree. Use for metacoder heat_tree coloured by abundance.
---

# Heat tree

`df2heattree()` is only `metacoder::heat_tree`. Node size is the number of taxa and colour is mean relative abundance. It stops when metacoder is not installed. There is no ggplot tree. The taxonomy graph is one tree: empty ranks are removed and their children reattached, the same way `metacoder::parse_phyloseq` drops taxa named `"NA"`. A `root` node is added only when the ranks would otherwise be a forest. Pass `tax` with Linnaean columns, or convert a phyloseq object with `phyloseq_to_metacoder()`, when the tidy labels are not already a full lineage.

```r
library(aRchiteutis)
path <- system.file("extdata", package = "aRchiteutis")
df <- get_counts(path, pattern = "decont_b", trim_char = "_")
df2heattree(df[df$clade == "S", ], top = 20)
```

UpSet presence across a legend column is `df2upset(df, group = "stage")`. That plot is `ComplexUpset::upset` and stops when ComplexUpset is not installed.

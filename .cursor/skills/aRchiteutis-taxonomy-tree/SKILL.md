---
name: aRchiteutis-taxonomy-tree
description: Build an ape taxonomy tree from NCBI taxids or a rank table with aRchiteutis taxids_to_tree and ranks_to_tree. Use when a phyloseq object needs a tree and no Newick was provided.
---

# Taxonomy tree

Do not call a hook. Use the package.

```r
library(aRchiteutis)

# Live NCBI efetch (needs network):
tr <- taxids_to_tree(c(562, 1578, 1213723))

# The same document, already fetched and shipped with the package:
xml <- paste(readLines(system.file("extdata", "ncbi_taxonomy.xml",
                                   package = "aRchiteutis"),
                       warn = FALSE), collapse = "\n")
lineage <- taxids_to_lineage(c(562, 1578), xml = xml)
tr <- ranks_to_tree(lineage)
```

The tree walks `kingdom/phylum/class/order/family/genus/species` and keeps unary rank nodes, so a full lineage stays seven levels deep. It is not an NCBI parent-id graph and not `hclust`. Empty ranks are filled with the last classified name. Pass the phylo object to `df2difftree(..., tree = tr)` or `phyloseq::phy_tree<-`.

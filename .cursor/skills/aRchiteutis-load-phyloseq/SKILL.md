---
name: aRchiteutis-load-phyloseq
description: Import a phyloseq object (or an unpacked list with otu_table, tax_table, sample_data) into aRchiteutis with from_phyloseq() so df2* plots can run. Use when the user mentions phyloseq, OTU/ASV tables from Bioconductor, or taxa_are_rows.
---

# Load phyloseq

```r
library(aRchiteutis)

# Real phyloseq object (requires the phyloseq package at runtime)
df <- from_phyloseq(physeq, taxa_rank = "Genus")

# Unpacked list (no phyloseq install needed)
df <- from_phyloseq(list(
  otu_table = otu,          # taxa x samples matrix
  tax_table = tax,          # optional; columns Kingdom..Species
  sample_data = sad         # optional; row names = samples
), taxa_rank = "Genus")
```

`taxa_rank` may be a name (`"Genus"`, `"Family"`, `"Species"`) or a clade letter (`"G"`). When `NULL`, taxon labels come from the OTU table names / finest non-empty rank, and `clade` is inferred from that rank.

If samples are rows in the OTU table, pass `taxa_are_rows = FALSE` on the list form.

Next: `aRchiteutis-transform` and a plot skill.

---
name: aRchiteutis-import-kaiju
description: Import Kaiju per-read output or a kaiju2table summary into phyloseq with aRchiteutis kaiju_to_phyloseq.
---

# Kaiju import

```r
library(aRchiteutis)

# kaiju2table summary (file, percent, reads, taxon_id, taxon_name):
ps <- kaiju_to_phyloseq("kaiju_summary.tsv", legend = "manifest.csv")

# per-read kaiju.out (C/U, read name, taxid, optional "; "-separated lineage):
ps <- kaiju_to_phyloseq("sample_kaiju.out")
```

The package example `inst/extdata/kaiju-summary-bee.tsv` is a kaiju2table whose taxon ids and read counts are the genus rows of the bundled Kraken2 report `m11`. Several files are several samples. Host and organelle names are dropped, and a rank-formula tree is attached.

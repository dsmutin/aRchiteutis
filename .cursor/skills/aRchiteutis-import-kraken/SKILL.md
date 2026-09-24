---
name: aRchiteutis-import-kraken
description: Import Kraken, Kraken2, KrakenUniq or Bracken reports into phyloseq with aRchiteutis kraken_to_phyloseq. Use for WGS taxonomic profiles that should become an OTU table, taxonomy table, sample data and a rank tree.
---

# Kraken-family import

No hook. `kraken_to_phyloseq()` reads the reports, drops Chordata / mitochondria / chloroplast, and attaches a rank-formula tree.

```r
library(aRchiteutis)
path <- system.file("extdata", package = "aRchiteutis")
legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")

ps <- kraken_to_phyloseq(
  path, pattern = "k2", legend = legend, trim_char = "_", rank = "G"
)
```

`rank` is `"G"` or `"S"`. `legend` may be a QIIME 2 manifest (`sample-id`, plus a `target` column) or a CSV whose first column is the sample id. Bracken `*.bracken` tables and KrakenUniq reports are accepted by the same function. A single file goes through `read_classifier_report()`.

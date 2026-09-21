---
name: aRchiteutis-load-kraken
description: Load Kraken2, Kaiju, or Bracken classifier reports into an aRchiteutis tidy count table with get_counts() and optional sample legend. Use when the user has a directory of k2/kaiju/bracken text reports.
---

# Load classifier reports

```r
library(aRchiteutis)
df <- get_counts(
  path = "path/to/reports",
  pattern = "",              # regex of file names to keep
  trim_char = "_",           # first field of the file name becomes `sample`
  keep_unclassified = TRUE,
  output_type = "kraken2",   # or "kaiju" / "bracken"
  legend = "legend.csv")     # CSV: first column matches sample names
```

Result columns: `taxa`, `clade`, `sample`, `N`, `amount`, `amount_cl`, plus legend.

Bundled example:

```r
path <- system.file("extdata", package = "aRchiteutis")
legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
df <- get_counts(path, pattern = "decont_b", legend = legend, trim_char = "_")
```

`get_kraken_taxonomy(path, pattern, extract_taxa, extract_clade)` scrapes unique clade/taxon rows when a taxonomy lookup is needed without counts.

Next: `aRchiteutis-transform`, then a plot skill, or the full `aRchiteutis` skill.

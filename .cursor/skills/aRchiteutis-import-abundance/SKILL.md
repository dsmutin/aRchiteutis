---
name: aRchiteutis-import-abundance
description: Import a taxa-by-sample abundance table whose rows are NCBI taxids into phyloseq with aRchiteutis abundance_taxid_to_phyloseq. Use when counts are already aggregated and lineages should come from a taxonomy tree.
---

# Abundance table with taxids

No hook. `abundance_taxid_to_phyloseq()` reads the table, resolves lineages with `taxids_to_lineage()`, and attaches a rank-formula tree.

```r
library(aRchiteutis)
counts <- system.file("extdata", "abundance-taxid-bee.tsv", package = "aRchiteutis")
xml <- paste(readLines(system.file("extdata", "ncbi_taxonomy.xml",
                                   package = "aRchiteutis"), warn = FALSE),
             collapse = "\n")
legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")

ps <- abundance_taxid_to_phyloseq(
  counts, metadata = legend, xml = xml, trim_char = "_"
)
```

Row names or a `taxid` / `taxonomy_id` / `taxon_id` column hold NCBI taxids. Remaining numeric columns are samples. Pass `xml` from an NCBI efetch document to avoid a network call; otherwise lineages are fetched. `metadata` is a QIIME 2-style manifest (`sample-id` or the first column, plus `target` when you have a grouping).

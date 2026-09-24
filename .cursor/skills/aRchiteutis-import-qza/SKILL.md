---
name: aRchiteutis-import-qza
description: Import several QIIME 2 artifacts (feature table, taxonomy, rooted tree) plus a sample manifest into phyloseq with aRchiteutis qza_to_phyloseq. Use for 16S or ITS .qza inputs.
---

# QIIME 2 qza import

No hook. `qza_to_phyloseq()` reads the artifacts. Pass a directory and it picks up `*table*.qza`, `*taxonomy*.qza`, `*tree*.qza` and a metadata TSV/CSV. Or pass each file.

```r
library(aRchiteutis)
qza <- system.file("extdata", "qza", package = "aRchiteutis")

ps <- qza_to_phyloseq(qza)
```

Metadata follows a QIIME 2 manifest: `sample-id` / `SampleID` (or the first column) plus annotation columns such as `target` or `BodySite`. A `#q2:types` row is ignored.

BIOM tables inside `.qza` need \pkg{biomformat}. When \pkg{qiime2R} is installed it is tried first. Chloroplast, mitochondria and Chordata features are dropped. The rooted tree is kept when its tips are the feature ids; otherwise a rank-formula tree is built from the taxonomy table.

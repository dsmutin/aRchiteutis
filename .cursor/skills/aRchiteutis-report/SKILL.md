---
name: aRchiteutis-report
description: Build a preliminary aRchiteutis MultiQC report with archi_report. Use when taxonomic profiles and a QIIME 2 legend should become a phyloseq object, a dropped-sample table, and captioned df2* figures.
---

# Preliminary report

No hook. `archi_report()` imports profiles, attaches a taxonomy tree, drops samples that fail the read-count floor or the rarefaction-curve check, and writes MultiQC custom content plus `architeutis_report.html`.

```r
library(aRchiteutis)
path <- system.file("extdata", package = "aRchiteutis")
legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
out <- tempfile("archi-report")

archi_report(
  path, legend, outdir = out,
  pattern = "m1[124]_", trim_char = "_", target = "stage",
  plots = c("composition", "alpha", "beta", "rarefaction", "heattree"),
  beta_method = "bray",          # or "jaccard", "aitchison", ...
  order_samples = "fpc",
  style = "box",                 # or "raincloud"
  curve_fraction = 0.8,          # terminal part of the QC curve
  curve_gain = 0.15,             # maximum terminal richness gain
  rarefaction_depths = c(1000L, 5000L),
  rarefaction_reps = 1L
)
```

`source` is `"auto"`, `"kraken"`, `"kaiju"`, `"qza"` or `"abundance"`. For a taxid table pass `source = "abundance"`, `counts`, and `xml`.

`plots` may be any of `composition`, `donut`, `barplot`, `alpha`, `beta`, `rarefaction`, `heattree`, `upset`, `difftree`. `beta_method` is any name from `archi_beta_methods()`. Grouped plots use the legend column named by `target` (`"stage"` on the bundled legend, `"target"` when the manifest already has that column).

The dropped-sample table is `dropped_samples.csv` and a real MultiQC custom
table (reasons `read_count` and `diversity_curve`). Importer `profile_reads`
are used for the read floor; the terminal curve check uses every taxon, not
the plotting `top`. Requested plots fail the report by default (`strict =
TRUE`) instead of disappearing. For a differential tree with more than two
targets, pass `contrast = c("control", "treatment")`.

Leave the closing line in place: this is a preliminary report only, not a final analysis.

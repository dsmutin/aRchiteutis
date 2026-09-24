# Preliminary report on the bundled Kraken2 bee-gut profiles.
# Rebuild with: Rscript inst/examples/kraken2_report.R
# Output is written to inst/examples/kraken2-report/.

library(aRchiteutis)

if (dir.exists("inst/extdata")) {
  path <- "inst/extdata"
  legend <- "inst/extdata/legend.csv"
  outdir <- "inst/examples/kraken2-report"
} else {
  path <- system.file("extdata", package = "aRchiteutis")
  legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
  outdir <- file.path(tempdir(), "kraken2-report")
}

archi_report(
  path, legend, outdir = outdir,
  pattern = "_k2\\.txt$",
  trim_char = "_",
  rank = "G",
  source = "kraken",
  target = "stage",
  plots = c("composition", "donut", "alpha", "beta", "rarefaction"),
  beta_method = "bray",
  order_samples = "fpc",
  style = "box",
  top = 15L,
  rarefaction_reps = 1L,
  curve_reps = 3L
)

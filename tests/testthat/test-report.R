test_that("a real Kraken2 subset becomes a preliminary report", {
  out <- tempfile("archi-report")
  path <- system.file("extdata", package = "aRchiteutis")
  legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
  res <- archi_report(
    path, legend, outdir = out, pattern = "m1[12]_", trim_char = "_",
    target = "stage", plots = c("composition", "rarefaction"),
    rarefaction_depths = c(500L, 2000L), rarefaction_reps = 1L, top = 8L
  )
  html <- paste(readLines(res$html, warn = FALSE), collapse = "\n")
  expect_true(grepl("preliminary report only", html, fixed = TRUE))
  expect_true(grepl("Stacked composition", html, fixed = TRUE))
  expect_true(file.exists(file.path(out, "dropped_samples.csv")))
  expect_true(file.exists(file.path(out, "multiqc", "archi-composition_mqc.png")))
  expect_true(file.exists(file.path(out, "multiqc", "archi-composition_mqc.yaml")))
  expect_true(file.exists(file.path(out, "multiqc", "architeutis-disclaimer_mqc.yaml")))
  expect_true(nrow(res$dropped) == 0L || all(res$dropped$reason %in% c("read_count", "diversity_curve")))
})

test_that("samples under the read floor are listed and excluded", {
  out <- tempfile("archi-report-drop")
  path <- system.file("extdata", package = "aRchiteutis")
  legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
  res <- archi_report(
    path, legend, outdir = out, pattern = "m1[12]_", trim_char = "_",
    target = "stage", plots = "composition", min_reads = 1e12, top = 5L
  )
  expect_true(all(c("m11", "m12") %in% res$dropped$sample))
  expect_true(all(res$dropped$reason == "read_count"))
  html <- paste(readLines(res$html, warn = FALSE), collapse = "\n")
  expect_true(grepl("preliminary report only", html, fixed = TRUE))
  expect_false(file.exists(file.path(out, "multiqc", "archi-composition_mqc.png")))
})

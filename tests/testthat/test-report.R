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
  expect_true(file.exists(file.path(out, "multiqc", "archi-composition.png")))
  expect_true(file.exists(file.path(out, "multiqc", "archi-composition_mqc.yaml")))
  expect_true(file.exists(file.path(out, "multiqc", "architeutis-dropped_mqc.tsv")))
  expect_true(file.exists(file.path(out, "multiqc", "architeutis-disclaimer_mqc.yaml")))
  mqc <- paste(readLines(
    file.path(out, "multiqc", "archi-composition_mqc.yaml"), warn = FALSE
  ), collapse = "\n")
  expect_true(grepl("Stacked composition", mqc, fixed = TRUE))
  expect_true(grepl("data:image/png;base64", mqc, fixed = TRUE))
  expect_true(nrow(res$dropped) == 0L || all(res$dropped$reason %in% c("read_count", "diversity_curve")))
  ps <- res$phyloseq
  otu <- archi_plain_otu(ps)
  tree <- if (inherits(ps, "phyloseq")) phyloseq::phy_tree(ps) else ps$phy_tree
  expect_setequal(tree$tip.label, rownames(otu))
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
  expect_false(file.exists(file.path(out, "multiqc", "archi-composition.png")))
})

test_that("terminal rarefaction flags an unsaturated sample using every taxon", {
  counts <- cbind(
    unsaturated = rep(1, 1000),
    saturated = c(rep(100, 10), rep(0, 990))
  )
  rownames(counts) <- paste0("taxon_", seq_len(nrow(counts)))
  df <- from_abundance(counts, clade = "G")
  flagged <- aRchiteutis:::archi_flag_samples(
    df, min_reads = 0, curve_gain = 0.15,
    curve_fraction = 0.8, curve_reps = 3L
  )
  expect_true("unsaturated" %in% flagged$sample)
  expect_false("saturated" %in% flagged$sample)
  expect_equal(flagged$reason[flagged$sample == "unsaturated"], "diversity_curve")
})

test_that("beta, UpSet and difftree panels build for two targets", {
  out <- tempfile("archi-report-groups")
  path <- system.file("extdata", package = "aRchiteutis")
  legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
  res <- archi_report(
    path, legend, outdir = out, pattern = "m(11|12|18|4)_",
    trim_char = "_", target = "stage",
    plots = c("beta", "upset", "difftree"), top = 8L
  )
  expect_true(all(file.exists(file.path(
    out, "multiqc", paste0("archi-", c("beta", "upset", "difftree"), ".png")
  ))))
  expect_false(file.exists(file.path(out, "Rplots.pdf")))
})

test_that("auto source keeps Kraken reports when a Kaiju fixture sits beside them", {
  out <- tempfile("archi-report-auto")
  res <- archi_report(
    system.file("extdata", package = "aRchiteutis"),
    system.file("extdata", "legend.csv", package = "aRchiteutis"),
    outdir = out, pattern = "", trim_char = "_", target = "stage",
    plots = "composition", top = 5L, curve_reps = 1L
  )
  expect_true(all(c("m11", "m12") %in% unique(from_phyloseq(res$phyloseq)$sample)))
  expect_false("kaiju" %in% res$dropped$sample)
})

test_that("requested report panels fail visibly in strict mode", {
  out <- tempfile("archi-report-strict")
  expect_error(
    archi_report(
      system.file("extdata", package = "aRchiteutis"),
      system.file("extdata", "legend.csv", package = "aRchiteutis"),
      outdir = out, pattern = "m1[12]_", trim_char = "_",
      target = "stage", plots = "beta", beta_method = "not-a-distance"
    ),
    "Report plot `beta` failed"
  )
})

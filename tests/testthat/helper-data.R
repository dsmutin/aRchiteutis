# Shared fixtures for the test suite.
#
# We build a single small `df` once (a 6-file subset of the shipped extdata
# reports) and reuse it across tests for speed. Species rows are retained so the
# alpha-diversity and sample-PCA tests have something to work with.

extdata_path <- function() {
  system.file("extdata", package = "aRchiteutis")
}

# Load the shared count table once when the helper is sourced.
suppressMessages(suppressWarnings({
  .archi_df <- get_counts(
    path = extdata_path(),
    pattern = "m(4|11|18|39|47|60)_",
    legend = file.path(extdata_path(), "legend.csv"),
    trim_char = "_")
}))

# Accessors keep individual tests terse.
archi_df <- function() .archi_df

# Table with species-level rows dropped (used by most composition / matrix
# plots, mirroring the main pipeline).
archi_df_nosp <- function() .archi_df[.archi_df$clade != "S", ]

# Convenience: build a taxa-by-sample matrix for the matrix-oriented plots.
archi_mat <- function(clade = "G", top = 12, scale = FALSE,
                      keep_sample_name = TRUE) {
  suppressMessages(suppressWarnings(
    df_untidy(archi_df(), clade = clade, top = top, scale = scale,
              keep_sample_name = keep_sample_name)))
}

# Run a base-graphics drawing expression on a throwaway PDF device so the tests
# never open a real device or leave files around.
expect_draws <- function(expr) {
  testthat::expect_no_error(
    withr::with_pdf(tempfile(fileext = ".pdf"), force(expr)))
}

# Build a ggplot without error (exercises the full grob construction).
expect_builds <- function(p) {
  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_no_error(
    suppressWarnings(ggplot2::ggplot_build(p)))
}

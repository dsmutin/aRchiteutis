# Tests for the pluggable beta-diversity distance backends added to
# df2beta() / df2beta_pcoa(): robCompositions (Aitchison) and adiv (Jaccard
# family), plus a regression check that the default Bray-Curtis path is
# unchanged.

# rgl (pulled in transitively by adiv) must run headless in CI.
withr::local_envvar(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

# Assert `d` is a valid symmetric sample-by-sample distance matrix.
expect_valid_beta <- function(d, n) {
  m <- as.matrix(d)
  testthat::expect_true(is.matrix(m))
  testthat::expect_equal(nrow(m), n)
  testthat::expect_equal(ncol(m), n)
  testthat::expect_true(all(diag(m) == 0))
  testthat::expect_true(isSymmetric(unname(m)))
  testthat::expect_true(all(is.finite(m)))
  testthat::expect_true(all(m >= 0))
}

n_samples <- function(df) nlevels(droplevels(factor(df$sample)))

# --- regression: default (bray) output is unchanged --------------------------

test_that("default method='bray' beta output is unchanged", {
  df <- archi_df()
  g <- df[df$clade == "G", ]

  d_default <- suppressMessages(df2beta(g, print_df = TRUE))
  d_explicit <- suppressMessages(
    df2beta(g, dist_function = abdiv::bray_curtis, print_df = TRUE))

  # method = "bray" with no dist_function reproduces the abdiv Bray-Curtis path.
  expect_identical(d_default, d_explicit)
  expect_valid_beta(d_default, n_samples(g))

  # PCoA default path is likewise the Bray-Curtis path.
  expect_builds(suppressMessages(df2beta_pcoa(g, add_legend = 7)))
})

# --- Aitchison (robCompositions) --------------------------------------------

test_that("aitchison backend produces valid distances (df input)", {
  skip_if_not_installed("robCompositions")
  df <- archi_df()
  g <- df[df$clade == "G", ]
  n <- n_samples(g)

  d_method <- suppressMessages(suppressWarnings(
    df2beta(g, method = "aitchison", print_df = TRUE)))
  expect_valid_beta(d_method, n)

  d_wrapper <- suppressMessages(suppressWarnings(
    df2beta_aitchison(g, print_df = TRUE)))
  expect_valid_beta(d_wrapper, n)
  expect_identical(d_method, d_wrapper)

  # Numeric pseudocount zero-replacement path also works and stays finite.
  d_pseudo <- suppressMessages(suppressWarnings(
    df2beta(g, method = "aitchison", zero_replace = 0.5, print_df = TRUE)))
  expect_valid_beta(d_pseudo, n)

  # Aitchison distances are not identical to Bray-Curtis distances.
  d_bray <- suppressMessages(df2beta(g, print_df = TRUE))
  expect_false(isTRUE(all.equal(unname(d_method), unname(d_bray))))
})

test_that("aitchison PCoA and heatmap render", {
  skip_if_not_installed("robCompositions")
  df <- archi_df()
  g <- df[df$clade == "G", ]

  expect_builds(suppressMessages(suppressWarnings(
    df2beta_pcoa(g, method = "aitchison", add_legend = 7))))

  expect_draws(suppressMessages(suppressWarnings(
    df2beta(g, method = "aitchison"))))
})

test_that("aitchison backend works from a phyloseq object", {
  skip_if_not_installed("robCompositions")
  skip_if_not_installed("phyloseq")
  ps <- archi_ps()

  d <- suppressMessages(suppressWarnings(
    df2beta_aitchison(ps, print_df = TRUE)))
  expect_valid_beta(d, phyloseq::nsamples(ps))
})

# --- adiv (Jaccard family) ---------------------------------------------------

test_that("adiv backend produces valid distances for each component", {
  skip_if_not_installed("adiv")
  df <- archi_df()
  g <- df[df$clade == "G", ]
  n <- n_samples(g)

  for (ix in c("jaccard", "turnover", "richness")) {
    d <- suppressMessages(df2beta(g, method = "adiv", adiv_index = ix,
                                  print_df = TRUE))
    expect_valid_beta(d, n)
  }

  d_wrapper <- suppressMessages(df2beta_adiv(g, index = "jaccard",
                                             print_df = TRUE))
  expect_valid_beta(d_wrapper, n)
  d_method <- suppressMessages(df2beta(g, method = "adiv",
                                       adiv_index = "jaccard", print_df = TRUE))
  expect_identical(d_wrapper, d_method)
})

test_that("adiv PCoA and heatmap render", {
  skip_if_not_installed("adiv")
  df <- archi_df()
  g <- df[df$clade == "G", ]

  expect_builds(suppressMessages(
    df2beta_pcoa(g, method = "adiv", adiv_index = "jaccard", add_legend = 7)))

  expect_draws(suppressMessages(df2beta_adiv(g, index = "turnover")))
})

test_that("adiv backend works from a phyloseq object", {
  skip_if_not_installed("adiv")
  skip_if_not_installed("phyloseq")
  ps <- archi_ps()

  d <- suppressMessages(df2beta_adiv(ps, index = "jaccard", print_df = TRUE))
  expect_valid_beta(d, phyloseq::nsamples(ps))
})

# --- backend dispatcher + missing-package errors -----------------------------

test_that("beta_dist dispatches and validates arguments", {
  set.seed(1)
  m <- matrix(sample(0:20, 24, replace = TRUE), nrow = 4,
              dimnames = list(paste0("s", 1:4), paste0("t", 1:6)))

  d <- beta_dist(m, method = "bray")
  expect_s3_class(d, "dist")
  expect_equal(attr(d, "Size"), 4L)

  # An explicit dist_function always wins over method.
  d2 <- beta_dist(m, method = "adiv", dist_function = abdiv::bray_curtis)
  expect_equal(as.matrix(d2), as.matrix(beta_dist(m, method = "bray")))

  expect_error(beta_dist(m, method = "not-a-method"))
})

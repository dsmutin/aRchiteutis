test_that("df2pca_sample returns a buildable ggplot", {
  mat <- archi_mat(clade = "S", top = FALSE, scale = "scale",
                   keep_sample_name = FALSE)
  expect_builds(df2pca_sample(mat, scale = FALSE, detect = "pupa"))
  expect_builds(df2pca_sample(mat, scale = FALSE))
})

test_that("df2pca_sp returns a buildable ggplot", {
  mat <- archi_mat(clade = "G", top = 12)
  expect_builds(df2pca_sp(mat, scale = TRUE))
})

test_that("df2heatmap draws without error", {
  mat <- archi_mat(clade = "G", top = 10)
  expect_draws(df2heatmap(mat, scale = "row", Colv = NA))
})

test_that("df2cluster draws dendrograms for taxa and samples", {
  mat <- archi_mat(clade = "G", top = 20, scale = "log2")
  expect_draws(df2cluster(mat, k_means = 4, use = "sp"))
  # k_means < 2 should still draw (no rectangles)
  expect_draws(df2cluster(mat, k_means = 0, use = "sample"))
})

test_that("df2clust2d returns a buildable ggplot", {
  df <- archi_df()
  g <- df[df$clade == "G", ]
  set.seed(1)
  expect_builds(suppressMessages(
    df2clust2d(g, legend_detect = "pupa", top = 30, k_means = 4)))
})

test_that("df2corrplot draws and returns the correlation matrix", {
  mat <- archi_mat(clade = "G", top = 12, scale = "scale")
  local({
    grDevices::pdf(tempfile(fileext = ".pdf"))
    on.exit(grDevices::dev.off())
    cmat <- df2corrplot(mat, k_means = 3)
    expect_true(is.matrix(cmat))
    expect_equal(dim(cmat), c(nrow(mat), nrow(mat)))
  })
})

test_that("df2chord returns a buildable ggraph plot (both edge colourings)", {
  mat <- archi_mat(clade = "G", top = 12, scale = "scale")
  set.seed(1)
  expect_builds(df2chord(mat, k_means = 3, coenf_level = 0.5))
  expect_builds(
    df2chord(mat, k_means = 3, coenf_level = 0.5, line_as_clusters = TRUE))
})

test_that("df2tsne returns a buildable ggplot", {
  mat <- archi_mat(clade = "G", top = 30, scale = "scale")
  set.seed(42)
  p <- suppressMessages(
    df2tsne(mat, k_means = 4, text_top = 5, perplexity = 5, max_iter = 100))
  expect_builds(p)
})

test_that("df2volcano returns a buildable ANCOM-BC2 ggplot", {
  skip_if_not_installed("ANCOMBC")
  df <- get_counts(
    extdata_path(),
    pattern = "m(11|12|13|18|4|39)_",
    legend = file.path(extdata_path(), "legend.csv"),
    trim_char = "_"
  )
  g <- df[df$clade == "G", ]
  keep <- names(sort(tapply(g$N, g$taxa, sum), decreasing = TRUE))[seq_len(40)]
  p <- df2volcano(g[g$taxa %in% keep, ], legend_detect = c("larvae", "pupa"),
                  treshhold_logAC = 0.5)
  expect_builds(p)
  expect_true("log2_lfc" %in% names(ggplot2::ggplot_build(p)$plot$data) ||
                "log2_lfc" %in% names(p$data))
})

test_that("df2volcano stops without ANCOMBC", {
  skip_if(requireNamespace("ANCOMBC", quietly = TRUE))
  g <- archi_df()
  g <- g[g$clade == "G", ]
  expect_error(df2volcano(g, legend_detect = c("larvae", "pupa")), "ANCOMBC")
})

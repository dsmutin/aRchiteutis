# Smoke tests for the NetCoMi bridge (df2netcomi / df2netcomi_graph).
# The whole file skips when NetCoMi is not installed.

test_that("df2netcomi builds and analyses a pearson network", {
  skip_if_not_installed("NetCoMi")

  mat <- archi_mat(clade = "G", top = 10, scale = FALSE)
  set.seed(1)
  res <- suppressWarnings(suppressMessages(
    df2netcomi(mat, measure = "pearson", thresh = 0.3, seed = 1)))

  expect_type(res, "list")
  expect_true(all(c("net", "props") %in% names(res)))
  expect_s3_class(res$net, "microNet")
  expect_s3_class(res$props, "microNetProps")
})

test_that("df2netcomi_graph bridges a NetCoMi network into a tbl_graph", {
  skip_if_not_installed("NetCoMi")

  mat <- archi_mat(clade = "G", top = 10, scale = FALSE)
  set.seed(1)
  res <- suppressWarnings(suppressMessages(
    df2netcomi(mat, measure = "pearson", thresh = 0.3, seed = 1)))

  set.seed(1)
  g <- df2netcomi_graph(res, threshold = 0.3)
  expect_s3_class(g, "tbl_graph")
  nodes <- tibble::as_tibble(tidygraph::activate(g, "nodes"))
  expect_true(all(c("community", "degree") %in% names(nodes)))
})

test_that("df2netcomi_graph can render via the df2ggraph engine", {
  skip_if_not_installed("NetCoMi")

  mat <- archi_mat(clade = "G", top = 10, scale = FALSE)
  set.seed(1)
  res <- suppressWarnings(suppressMessages(
    df2netcomi(mat, measure = "pearson", thresh = 0.3, seed = 1)))

  set.seed(1)
  p <- df2netcomi_graph(res, threshold = 0.3, plot = TRUE)
  expect_builds(p)
})

test_that("df2netcomi_graph runs end-to-end from a bare matrix", {
  skip_if_not_installed("NetCoMi")

  mat <- archi_mat(clade = "G", top = 10, scale = FALSE)
  set.seed(1)
  g <- suppressWarnings(suppressMessages(
    df2netcomi_graph(mat, measure = "pearson", threshold = 0.3, seed = 1)))
  expect_s3_class(g, "tbl_graph")
})

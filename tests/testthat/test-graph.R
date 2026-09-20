# Tests for the tidygraph -> ggraph engine (df2graph / df2ggraph).

# Helper: node / edge tibbles from a tbl_graph.
gnodes <- function(g) tibble::as_tibble(tidygraph::activate(g, "nodes"))
gedges <- function(g) tibble::as_tibble(tidygraph::activate(g, "edges"))

test_that("df2graph returns an annotated tbl_graph from a matrix", {
  mat <- archi_mat(clade = "G", top = 12, scale = "scale")
  set.seed(1)
  g <- df2graph(mat, threshold = 0.3)

  expect_s3_class(g, "tbl_graph")
  expect_equal(igraph::gorder(g), nrow(mat))

  nodes <- gnodes(g)
  # Community + centrality annotations must be present on the nodes.
  expect_true(all(c("name", "community", "degree", "betweenness",
                    "closeness") %in% names(nodes)))
  expect_s3_class(nodes$community, "factor")
  expect_true(is.numeric(nodes$degree))

  # Edges carry the signed correlation, its magnitude and sign.
  edges <- gedges(g)
  expect_true(all(c("weight", "abs_weight", "sign") %in% names(edges)))
  expect_true(all(edges$abs_weight >= 0.3 - 1e-9))
  expect_setequal(unique(edges$sign), intersect(c("positive", "negative"),
                                                unique(edges$sign)))
})

test_that("df2graph honours the threshold (higher threshold -> fewer edges)", {
  mat <- archi_mat(clade = "G", top = 12, scale = "scale")
  set.seed(1)
  g_lo <- df2graph(mat, threshold = 0.2)
  g_hi <- df2graph(mat, threshold = 0.8)
  expect_gte(igraph::gsize(g_lo), igraph::gsize(g_hi))
})

test_that("df2graph accepts different community methods", {
  mat <- archi_mat(clade = "G", top = 12, scale = "scale")
  set.seed(2)
  g <- df2graph(mat, threshold = 0.3, cluster_method = "walktrap")
  expect_s3_class(g, "tbl_graph")
  expect_true("community" %in% names(gnodes(g)))
})

test_that("df2graph works on a phyloseq object", {
  skip_if_not_installed("phyloseq")
  ps <- archi_ps()
  set.seed(1)
  g <- df2graph(ps, clade = "G", threshold = 0.3)
  expect_s3_class(g, "tbl_graph")
  expect_true(all(c("community", "degree") %in% names(gnodes(g))))
})

test_that("df2ggraph returns a buildable ggplot from a tbl_graph", {
  mat <- archi_mat(clade = "G", top = 12, scale = "scale")
  set.seed(1)
  g <- df2graph(mat, threshold = 0.3)
  set.seed(1)
  expect_builds(df2ggraph(g))
})

test_that("df2ggraph returns a buildable ggplot directly from a matrix", {
  mat <- archi_mat(clade = "G", top = 12, scale = "scale")
  set.seed(3)
  expect_builds(df2ggraph(mat, threshold = 0.3, layout = "kk"))
})

test_that("df2ggraph returns a buildable ggplot from a phyloseq object", {
  skip_if_not_installed("phyloseq")
  ps <- archi_ps()
  set.seed(1)
  expect_builds(df2ggraph(ps, clade = "G", threshold = 0.3))
})

test_that("df2graph is deterministic given a fixed seed", {
  mat <- archi_mat(clade = "G", top = 12, scale = "scale")
  set.seed(7)
  g1 <- df2graph(mat, threshold = 0.3)
  set.seed(7)
  g2 <- df2graph(mat, threshold = 0.3)
  expect_equal(gnodes(g1)$community, gnodes(g2)$community)
})

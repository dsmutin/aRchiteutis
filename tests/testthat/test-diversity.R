test_that("raincloud style builds alpha and composition boxplots", {
  skip_if_not_installed("ggviolinbox")
  sp <- archi_df()
  sp <- sp[sp$clade == "S", ]
  expect_builds(df2alpha_summary(sp, split_by = 7, style = "raincloud"))
  expect_builds(df2alpha(sp, style = "raincloud"))
  g <- archi_df()
  g <- g[g$clade == "G", ]
  expect_builds(df2barplot(g, style = "raincloud"))
})

test_that("df2alpha_summary returns a buildable ggplot", {
  df <- archi_df()
  sp <- df[df$clade == "S", ]
  expect_builds(df2alpha_summary(sp, split_by = 7, add_legend = 7:8))
  expect_builds(df2alpha_summary(sp))
})

test_that("df2alpha returns a buildable ggplot", {
  df <- archi_df()
  sp <- df[df$clade == "S", ]
  expect_builds(df2alpha(sp, add_legend = 7))
  expect_builds(df2alpha(sp, split_by = 7))
})

test_that("df2beta draws a heatmap and can return the distance matrix", {
  df <- archi_df()
  g <- df[df$clade == "G", ]

  d <- suppressMessages(df2beta(g, print_df = TRUE))
  expect_true(is.matrix(d))
  expect_equal(nrow(d), ncol(d))
  expect_equal(nrow(d), nlevels(droplevels(df$sample)))

  expect_draws(suppressMessages(df2beta(g, add_legend = 7:8, add_labels = 7)))
})

test_that("df2beta_bray is a Bray-Curtis wrapper around df2beta", {
  df <- archi_df()
  g <- df[df$clade == "G", ]

  d1 <- suppressMessages(df2beta_bray(g, print_df = TRUE))
  d2 <- suppressMessages(
    df2beta(g, dist_function = abdiv::bray_curtis, print_df = TRUE))
  expect_equal(d1, d2)

  expect_draws(suppressMessages(df2beta_bray(g)))
})

test_that("beta methods cover vegan distances and Aitchison aDist", {
  skip_if_not_installed("vegan")
  skip_if_not_installed("robCompositions")
  g <- archi_df()
  g <- g[g$clade == "G", ]
  expect_true("aitchison" %in% archi_beta_methods())
  bray <- archi_distance_matrix(g, "bray")
  jac <- archi_distance_matrix(g, "jaccard")
  ait <- archi_distance_matrix(g, "aitchison")
  for (m in list(bray, jac, ait)) {
    expect_true(isSymmetric(m))
    expect_equal(nrow(m), ncol(m))
    expect_true(all(is.finite(m)))
    expect_equal(unname(diag(m)), rep(0, nrow(m)))
  }
  expect_true(attr(ait, "zero_method") %in%
                c("none", "robCompositions::impRZilr", "multRepl_Martin-Fernandez_2003"))
  expect_error(archi_distance_matrix(g, "not-a-distance"), "Unknown distance")
  expect_builds(suppressMessages(df2beta_pcoa(g, method = "jaccard", add_legend = 7)))
  d <- suppressMessages(df2beta(g, method = "euclidean", print_df = TRUE))
  expect_equal(unname(diag(d)), rep(0, nrow(d)))

  counts <- t(df_untidy(g, amount_from = "N", drop_unclassified = TRUE))
  expect_equal(
    unclass(bray),
    unclass(as.matrix(vegan::vegdist(counts, method = "bray"))),
    tolerance = 1e-12
  )
  expect_warning(chao <- archi_distance_matrix(g, "chao"), NA)
  expect_true(all(is.finite(chao)))
})

test_that("df2beta_pcoa returns a buildable ggplot", {
  df <- archi_df()
  g <- df[df$clade == "G", ]
  expect_builds(suppressMessages(df2beta_pcoa(g, add_legend = 7)))
  expect_builds(suppressMessages(df2beta_pcoa(g, add_legend = 7, add_ellipse = 7)))
})

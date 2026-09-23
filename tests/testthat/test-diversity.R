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

test_that("df2beta_pcoa returns a buildable ggplot", {
  df <- archi_df()
  g <- df[df$clade == "G", ]
  expect_builds(suppressMessages(df2beta_pcoa(g, add_legend = 7)))
  expect_builds(suppressMessages(df2beta_pcoa(g, add_legend = 7, add_ellipse = 7)))
})

# Tests for the optional ggviolinbox rendering added to df2alpha(),
# df2alpha_summary() and df2barplot(). The whole file is skipped when
# ggviolinbox is not installed so the default (violinbox = FALSE) suite still
# runs everywhere; those defaults are additionally regression-checked here.
testthat::skip_if_not_installed("ggviolinbox")

test_that("df2alpha_summary supports violinbox rendering", {
  df <- archi_df()
  sp <- df[df$clade == "S", ]

  # regression: default path is still a buildable ggplot.
  expect_builds(df2alpha_summary(sp))

  # combined violinboxplot, both the split and non-split layouts.
  p_split <- df2alpha_summary(sp, split_by = 7, add_legend = 7:8,
                              violinbox = TRUE)
  expect_s3_class(p_split, "ggplot")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p_split)))

  p_flat <- df2alpha_summary(sp, violinbox = TRUE)
  expect_s3_class(p_flat, "ggplot")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p_flat)))

  # "halves" mode + side selection plumbed through.
  p_halves <- df2alpha_summary(sp, split_by = 7, violinbox = "halves",
                               box_side = "right", violin_side = "left")
  expect_s3_class(p_halves, "ggplot")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p_halves)))
})

test_that("df2alpha supports violinbox rendering", {
  df <- archi_df()
  sp <- df[df$clade == "S", ]

  expect_builds(df2alpha(sp, add_legend = 7))

  p_split <- df2alpha(sp, split_by = 7, violinbox = TRUE)
  expect_s3_class(p_split, "ggplot")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p_split)))

  p_flat <- df2alpha(sp, add_legend = 7, violinbox = "combined")
  expect_s3_class(p_flat, "ggplot")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p_flat)))

  p_sides <- df2alpha(sp, split_by = 7, violinbox = TRUE,
                      box_side = "right", violin_side = "left")
  expect_s3_class(p_sides, "ggplot")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p_sides)))
})

test_that("df2barplot supports violinbox rendering", {
  dfT <- suppressMessages(df_taxa_trim(archi_df_nosp(), top_taxa = 8))

  expect_builds(df2barplot(dfT))

  p <- df2barplot(dfT, violinbox = TRUE)
  expect_s3_class(p, "ggplot")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p)))

  p_halves <- df2barplot(dfT, violinbox = "halves",
                         box_side = "right", violin_side = "left")
  expect_s3_class(p_halves, "ggplot")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p_halves)))
})

test_that("violinbox with an invalid side is rejected", {
  df <- archi_df()
  sp <- df[df$clade == "S", ]
  expect_error(df2alpha(sp, split_by = 7, violinbox = TRUE,
                        box_side = "up"),
               "left")
})

test_that("violinbox works on a phyloseq object", {
  testthat::skip_if_not_installed("phyloseq")
  ps <- archi_ps()

  p <- suppressMessages(suppressWarnings(
    df2alpha_summary(ps, split_by = 7, add_legend = 7:8, violinbox = TRUE)))
  expect_s3_class(p, "ggplot")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p)))

  p2 <- suppressMessages(suppressWarnings(
    df2barplot(ps, violinbox = TRUE)))
  expect_s3_class(p2, "ggplot")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p2)))
})

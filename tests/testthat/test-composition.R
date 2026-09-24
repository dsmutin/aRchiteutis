test_that("df2donut returns a buildable ggplot", {
  dfT <- suppressMessages(df_taxa_trim(archi_df_nosp(), top_taxa = 8))
  expect_builds(df2donut(dfT))
})

test_that("composition bars default to an FPC sample order", {
  df <- archi_df_nosp()
  dfT <- df_taxa_trim(df, top_taxa = 6)
  plain <- df2composition(dfT, order_samples = "none")
  input_levels <- unique(as.character(df_tidy_drop_unclassified(dfT)$sample))
  expect_equal(levels(plain$data$sample), input_levels)
  fpc <- df2composition(dfT)
  expect_equal(sort(levels(fpc$data$sample)), sort(input_levels))
  expect_equal(levels(fpc$data$sample), archi_order_sample_levels(fpc$data, "fpc"))
  for (method in c("hclust", "abundance", "alpha")) {
    expect_builds(df2composition(dfT, order_samples = method))
  }
})

test_that("df2composition returns a buildable ggplot", {
  dfT <- suppressMessages(df_taxa_trim(archi_df_nosp(), top_taxa = 8))
  expect_builds(df2composition(dfT))
})

test_that("df2barplot returns a buildable ggplot", {
  dfT <- suppressMessages(df_taxa_trim(archi_df_nosp(), top_taxa = 8))
  expect_builds(df2barplot(dfT))
})

test_that("df2donut returns a buildable ggplot", {
  dfT <- suppressMessages(df_taxa_trim(archi_df_nosp(), top_taxa = 8))
  expect_builds(df2donut(dfT))
})

test_that("df2composition returns a buildable ggplot", {
  dfT <- suppressMessages(df_taxa_trim(archi_df_nosp(), top_taxa = 8))
  expect_builds(df2composition(dfT))
})

test_that("df2barplot returns a buildable ggplot", {
  dfT <- suppressMessages(df_taxa_trim(archi_df_nosp(), top_taxa = 8))
  expect_builds(df2barplot(dfT))
})

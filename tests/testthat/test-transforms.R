test_that("get_counts reads the shipped reports into the documented shape", {
  df <- archi_df()
  expect_s3_class(df, "tbl_df")
  expect_true(all(c("taxa", "clade", "sample", "N", "amount", "amount_cl",
                    "stage", "hive") %in% names(df)))
  expect_equal(nlevels(df$sample), 6L)
  expect_type(df$N, "double")
})

test_that("df_untidy pivots to a taxa-by-sample matrix and honours top/trim", {
  df <- archi_df()
  mat <- df_untidy(df, clade = "G", top = 5)
  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), 5L)
  expect_equal(ncol(mat), nlevels(droplevels(df$sample)))
  expect_false(anyNA(mat))

  # `trim` is a legacy alias for `top`
  mat_trim <- df_untidy(df, clade = "G", trim = 5)
  expect_equal(dim(mat_trim), dim(mat))
})

test_that("df_get_top_taxa returns a filtered long data frame, not a vector", {
  df <- archi_df()
  top <- suppressMessages(df_get_top_taxa(df, clade = "G", top = 10))
  expect_s3_class(top, "tbl_df")
  expect_true(all(names(df) %in% names(top)))
  expect_lte(length(unique(top$taxa)), 10L)
})

test_that("df_tidy_drop_unclassified removes unclassified taxa", {
  df <- archi_df()
  dropped <- df_tidy_drop_unclassified(df)
  expect_false(any(grepl("unclassified", dropped$taxa)))
})

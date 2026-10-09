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

test_that("composition tree is a ggtree fan with abundance boxes", {
  dfT <- archi_df_nosp()
  p <- df2composition_tree(dfT, top = 8)
  expect_builds(p)
  expect_s3_class(p, "ggtree")
})

test_that("composition tree keeps seven ranks when tax is supplied", {
  dfT <- archi_df_nosp()
  taxa <- unique(as.character(df_get_top_taxa(dfT, top = 8)$taxa))
  tax <- data.frame(
    taxa = taxa,
    kingdom = "Bacteria",
    phylum = rep(c("Bacillota", "Pseudomonadota"), length.out = length(taxa)),
    class = rep(c("Bacilli", "Gammaproteobacteria"), length.out = length(taxa)),
    order = taxa,
    family = taxa,
    genus = taxa,
    species = taxa,
    stringsAsFactors = FALSE
  )
  built <- aRchiteutis:::archi_composition_tree(dfT[dfT$taxa %in% taxa, ], tax, "phylum")
  expect_gte(max(ape::node.depth.edgelength(built$tree)), 6)
  expect_builds(df2composition_tree(dfT, tax = tax, top = 8))
})

test_that("df2composition returns a buildable ggplot", {
  dfT <- suppressMessages(df_taxa_trim(archi_df_nosp(), top_taxa = 8))
  expect_builds(df2composition(dfT))
})

test_that("df2barplot returns a buildable ggplot", {
  dfT <- suppressMessages(df_taxa_trim(archi_df_nosp(), top_taxa = 8))
  expect_builds(df2barplot(dfT))
})

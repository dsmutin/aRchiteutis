test_that("from_abundance turns a wide matrix into a tidy aRchiteutis table", {
  mat <- matrix(c(10, 5, 1, 20, 0, 4), nrow = 2,
                dimnames = list(c("Lactobacillus", "Bifidobacterium"),
                                c("s1", "s2", "s3")))
  df <- from_abundance(mat, clade = "G")
  expect_s3_class(df, "tbl_df")
  expect_equal(sort(names(df)[1:6]),
               sort(c("taxa", "clade", "sample", "N", "amount", "amount_cl")))
  expect_equal(nlevels(df$sample), 3L)
  expect_equal(unique(df$clade), "G")
  expect_equal(sum(df$N[df$sample == "s1"]), 15)
  expect_equal(df$amount[df$sample == "s1" & df$taxa == "Lactobacillus"], 10 / 15)
})

test_that("from_abundance accepts a first-column taxa data frame and a legend", {
  tab <- data.frame(taxa = c("A", "B"), s1 = c(1, 3), s2 = c(2, 2),
                    stringsAsFactors = FALSE)
  legend <- data.frame(sample = c("s1", "s2"), stage = c("larvae", "pupa"),
                       stringsAsFactors = TRUE)
  df <- from_abundance(tab, clade = "S", legend = legend)
  expect_true("stage" %in% names(df))
  expect_equal(as.character(unique(df$stage[df$sample == "s2"])), "pupa")
})

test_that("as_archi maps common aliases and fills missing amount", {
  raw <- data.frame(otu = c("x", "y"), sample = c("a", "a"), count = c(4, 6))
  df <- as_archi(raw, clade = "G")
  expect_equal(df$taxa, c("x", "y"))
  expect_equal(df$N, c(4, 6))
  expect_equal(df$amount, c(0.4, 0.6))
  expect_equal(unique(df$clade), "G")
})

test_that("from_phyloseq accepts an unpacked list without phyloseq installed", {
  otu <- matrix(c(8, 2, 1, 9), nrow = 2,
                dimnames = list(c("g1", "g2"), c("s1", "s2")))
  tax <- data.frame(Genus = c("Lactobacillus", "Gilliamella"),
                    row.names = c("g1", "g2"), stringsAsFactors = FALSE)
  sad <- data.frame(stage = c("larvae", "pupa"), row.names = c("s1", "s2"))
  df <- from_phyloseq(list(otu_table = otu, tax_table = tax, sample_data = sad),
                      taxa_rank = "Genus")
  expect_true(all(c("Lactobacillus", "Gilliamella") %in% df$taxa))
  expect_true("stage" %in% names(df))
  expect_equal(unique(df$clade), "G")
})

test_that("python2r reads the bundled CSV and plotting functions accept it", {
  csv <- system.file("extdata", "python_abundance.csv", package = "aRchiteutis")
  df <- python2r(csv, clade = "G")
  expect_s3_class(df, "tbl_df")
  expect_equal(nlevels(df$sample), 4L)
  expect_true("Lactobacillus" %in% df$taxa)
  expect_builds(df2composition(df))
  expect_builds(df2barplot(df))
})

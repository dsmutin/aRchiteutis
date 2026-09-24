test_that("real QIIME 2 moving-pictures qza files become phyloseq", {
  skip_if_not_installed("biomformat")
  qza <- system.file("extdata", "qza", package = "aRchiteutis")
  ps <- qza_to_phyloseq(qza)
  otu <- if (inherits(ps, "phyloseq")) as.matrix(phyloseq::otu_table(ps)) else ps$otu_table
  tax <- if (inherits(ps, "phyloseq")) {
    as.data.frame(phyloseq::tax_table(ps))
  } else {
    as.data.frame(ps$tax_table)
  }
  sam <- if (inherits(ps, "phyloseq")) as.data.frame(phyloseq::sample_data(ps)) else ps$sample_data
  tree <- if (inherits(ps, "phyloseq")) phyloseq::phy_tree(ps) else ps$phy_tree
  expect_true(nrow(otu) > 50)
  expect_true(all(c("L1S8", "L1S57") %in% colnames(otu)))
  expect_true("genus" %in% names(tax))
  expect_true(any(grepl("Coprococcus", tax$genus)))
  expect_true("BodySite" %in% names(sam))
  expect_equal(as.character(sam["L1S8", "BodySite"]), "gut")
  expect_s3_class(tree, "phylo")
  expect_true(all(tree$tip.label %in% rownames(otu)))
  expect_false(any(grepl("Chloroplast|Mitochondria", tax$family, ignore.case = TRUE)))
})

test_that("several feature tables merge without losing taxa or samples", {
  a <- tempfile(fileext = ".tsv")
  b <- tempfile(fileext = ".tsv")
  taxonomy <- tempfile(fileext = ".tsv")
  metadata <- tempfile(fileext = ".tsv")
  writeLines(c("Feature ID\tsample-a", "f1\t10", "f2\t2"), a)
  writeLines(c("# Constructed from biom", "#OTU ID\tsample-b", "f1\t3", "f2\t8"), b)
  writeLines(c(
    "Feature ID\tTaxon",
    "f1\tk__Bacteria; p__Bacillota; c__Bacilli; o__Lactobacillales; f__Lactobacillaceae; g__Lactobacillus",
    "f2\tk__Bacteria; p__Pseudomonadota; c__Gammaproteobacteria; o__Enterobacterales; f__Enterobacteriaceae; g__Escherichia"
  ), taxonomy)
  writeLines(c("sample-id\ttarget", "sample-a\tA", "sample-b\tB"), metadata)

  ps <- qza_to_phyloseq(c(a, b), taxonomy = taxonomy, metadata = metadata)
  otu <- if (inherits(ps, "phyloseq")) as.matrix(phyloseq::otu_table(ps)) else ps$otu_table
  tree <- if (inherits(ps, "phyloseq")) phyloseq::phy_tree(ps) else ps$phy_tree
  expect_equal(unname(otu["f1", c("sample-a", "sample-b")]), c(10, 3))
  expect_equal(unname(otu["f2", c("sample-a", "sample-b")]), c(2, 8))
  expect_setequal(tree$tip.label, c("f1", "f2"))
})

test_that("qza extraction cache is keyed by content, not basename", {
  a <- tempfile()
  b <- tempfile()
  dir.create(a)
  dir.create(b)
  file.copy(
    system.file("extdata", "qza", "moving-pictures-table.qza", package = "aRchiteutis"),
    file.path(a, "table.qza")
  )
  file.copy(
    system.file("extdata", "qza", "moving-pictures-taxonomy.qza", package = "aRchiteutis"),
    file.path(b, "table.qza")
  )
  one <- aRchiteutis:::archi_qza_extract(file.path(a, "table.qza"))
  two <- aRchiteutis:::archi_qza_extract(file.path(b, "table.qza"))
  expect_false(identical(one, two))
})

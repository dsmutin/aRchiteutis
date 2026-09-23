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

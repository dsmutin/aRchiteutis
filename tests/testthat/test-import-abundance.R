test_that("a real taxid abundance table becomes phyloseq", {
  counts <- system.file("extdata", "abundance-taxid-bee.tsv", package = "aRchiteutis")
  xml <- paste(readLines(system.file("extdata", "ncbi_taxonomy.xml",
                                     package = "aRchiteutis"), warn = FALSE),
               collapse = "\n")
  legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
  ps <- abundance_taxid_to_phyloseq(counts, metadata = legend, xml = xml,
                                    trim_char = "_")
  otu <- if (inherits(ps, "phyloseq")) as.matrix(phyloseq::otu_table(ps)) else ps$otu_table
  tax <- if (inherits(ps, "phyloseq")) {
    as.data.frame(phyloseq::tax_table(ps))
  } else {
    as.data.frame(ps$tax_table)
  }
  sam <- if (inherits(ps, "phyloseq")) as.data.frame(phyloseq::sample_data(ps)) else ps$sample_data
  tree <- if (inherits(ps, "phyloseq")) phyloseq::phy_tree(ps) else ps$phy_tree
  expect_equal(ncol(otu), 2L)
  expect_true(all(c("m11", "m12") %in% colnames(otu)))
  expect_equal(otu["tax_562", "m11"], 66)
  expect_equal(otu["tax_1578", "m12"], 14)
  expect_true(any(grepl("Escherichia coli", tax$species)))
  expect_equal(as.character(sam["m11", "stage"]), "larvae")
  expect_s3_class(tree, "phylo")
  expect_true(ape::Ntip(tree) >= 2)
})

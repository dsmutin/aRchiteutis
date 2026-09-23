test_that("a Kaiju summary table becomes phyloseq", {
  f <- system.file("extdata", "kaiju-summary-bee.tsv", package = "aRchiteutis")
  ps <- kaiju_to_phyloseq(f)
  otu <- if (inherits(ps, "phyloseq")) as.matrix(phyloseq::otu_table(ps)) else ps$otu_table
  expect_equal(ncol(otu), 1L)
  expect_true(nrow(otu) >= 5)
  expect_true(sum(otu) > 0)
  tree <- if (inherits(ps, "phyloseq")) phyloseq::phy_tree(ps) else ps$phy_tree
  expect_s3_class(tree, "phylo")
})

test_that("per-read kaiju.out rows aggregate by taxid", {
  src <- system.file("extdata", "kaiju-summary-bee.tsv", package = "aRchiteutis")
  tab <- utils::read.delim(src, stringsAsFactors = FALSE)
  lines <- c(
    paste("C", "read1", tab$taxon_id[[1]], tab$taxon_name[[1]], sep = "\t"),
    paste("C", "read2", tab$taxon_id[[1]], tab$taxon_name[[1]], sep = "\t"),
    paste("U", "read3", "0", "", sep = "\t")
  )
  tmp <- tempfile(fileext = ".out")
  writeLines(lines, tmp)
  got <- aRchiteutis:::archi_read_kaiju_file(tmp)
  expect_equal(nrow(got), 1L)
  expect_equal(got$reads, 2)
})

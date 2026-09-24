test_that("a Kaiju summary table becomes phyloseq", {
  f <- system.file("extdata", "kaiju-example.tsv", package = "aRchiteutis")
  ps <- kaiju_to_phyloseq(f)
  otu <- if (inherits(ps, "phyloseq")) as.matrix(phyloseq::otu_table(ps)) else ps$otu_table
  expect_equal(ncol(otu), 1L)
  expect_equal(nrow(otu), 3L)
  expect_equal(otu["tax_55507", "kaiju"], 308688)
  tree <- if (inherits(ps, "phyloseq")) phyloseq::phy_tree(ps) else ps$phy_tree
  expect_s3_class(tree, "phylo")
  expect_setequal(tree$tip.label, rownames(otu))
  sam <- if (inherits(ps, "phyloseq")) {
    as.data.frame(phyloseq::sample_data(ps))
  } else {
    ps$sample_data
  }
  expect_gt(sam["kaiju", "profile_reads"], sum(otu))
})

test_that("per-read kaiju.out rows aggregate by taxid", {
  lines <- c(
    paste("C", "RL|S1|R549", "55507", "259", "55507,",
          "WP_072934244.1,", "NTMTAGLVASYIGRITAAWNAE,", sep = "\t"),
    paste("C", "read2", "55507", sep = "\t"),
    paste("U", "read3", "0", "", sep = "\t")
  )
  tmp <- tempfile(fileext = ".out")
  writeLines(lines, tmp)
  got <- aRchiteutis:::archi_read_kaiju_file(tmp)
  expect_equal(nrow(got), 1L)
  expect_equal(got$reads, 2)
  expect_equal(unname(attr(got, "profile_reads")), 3)
})

test_that("full Kaiju paths discard unranked clades without shifting ranks", {
  genus <- aRchiteutis:::archi_parse_kaiju_lineage(paste(
    "cellular organisms", "Bacteria", "Terrabacteria group", "Bacillota",
    "Bacilli", "Lactobacillales", "Lactobacillaceae", "Lactobacillus",
    sep = ";"
  ))
  expect_equal(unname(genus[c("kingdom", "phylum", "class", "order", "family", "genus")]),
               c("Bacteria", "Bacillota", "Bacilli", "Lactobacillales",
                 "Lactobacillaceae", "Lactobacillus"))

  species <- aRchiteutis:::archi_parse_kaiju_lineage(paste(
    "cellular organisms", "Bacteria", "Pseudomonadota",
    "Gammaproteobacteria", "Enterobacterales", "Enterobacteriaceae",
    "Escherichia", "Escherichia coli", sep = ";"
  ))
  expect_equal(unname(species[c("kingdom", "phylum", "genus", "species")]),
               c("Bacteria", "Pseudomonadota", "Escherichia", "Escherichia coli"))
})

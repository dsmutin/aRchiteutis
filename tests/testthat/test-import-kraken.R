test_that("real Kraken2 reports become a phyloseq-shaped object", {
  path <- system.file("extdata", package = "aRchiteutis")
  legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
  ps <- kraken_to_phyloseq(path, pattern = "m1[12]_.*k2", legend = legend,
                           trim_char = "_", rank = "G")
  otu <- if (inherits(ps, "phyloseq")) phyloseq::otu_table(ps) else ps$otu_table
  tax <- if (inherits(ps, "phyloseq")) {
    as.data.frame(phyloseq::tax_table(ps))
  } else {
    as.data.frame(ps$tax_table)
  }
  sam <- if (inherits(ps, "phyloseq")) as.data.frame(phyloseq::sample_data(ps)) else ps$sample_data
  tree <- if (inherits(ps, "phyloseq")) phyloseq::phy_tree(ps) else ps$phy_tree
  otu <- as.matrix(otu)
  expect_true(nrow(otu) > 5)
  expect_true(all(c("m11", "m12") %in% colnames(otu)))
  expect_true("genus" %in% names(tax))
  expect_true("stage" %in% names(sam))
  expect_s3_class(tree, "phylo")
  expect_true(ape::Ntip(tree) >= 2)
  one <- read_classifier_report(
    list.files(path, pattern = "m11_.*k2", full.names = TRUE)[[1]],
    rank = "G"
  )
  expect_true(all(one$rank == "G"))
  expect_true(all(one$reads > 0))
  expect_false(anyNA(one$phylum))
})

test_that("Bracken tables and KrakenUniq reports reuse the same reader", {
  src <- list.files(system.file("extdata", package = "aRchiteutis"),
                    pattern = "m11_.*k2", full.names = TRUE)[[1]]
  genus <- read_classifier_report(src, rank = "G")
  br <- tempfile(fileext = ".bracken")
  utils::write.table(
    data.frame(name = genus$name, taxonomy_id = genus$taxid,
               taxonomy_lvl = "G", new_est_reads = genus$reads),
    br, sep = "\t", quote = FALSE, row.names = FALSE
  )
  back <- read_classifier_report(br, rank = "G")
  expect_equal(sum(back$reads), sum(genus$reads))
  expect_true(all(back$rank == "G"))

  raw <- utils::read.table(src, sep = "\t", quote = "", fill = TRUE,
                          comment.char = "", stringsAsFactors = FALSE)
  raw <- utils::head(raw, 30)
  uniq <- data.frame(raw[, 1:3], kmers = 1, dup = 1, taxid = raw[, 5],
                     rank = raw[, 4], name = raw[, 6], stringsAsFactors = FALSE)
  ufile <- tempfile(fileext = ".report")
  utils::write.table(uniq, ufile, sep = "\t", quote = FALSE,
                    row.names = FALSE, col.names = FALSE)
  got <- read_classifier_report(ufile, rank = "G")
  expect_true(nrow(got) >= 1)
  expect_true(all(got$taxid > 0))
})

test_that("real Kraken2 reports become a phyloseq-shaped object", {
  path <- system.file("extdata", package = "aRchiteutis")
  legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
  ps <- kraken_to_phyloseq(path, pattern = "m1[12]_.*k2", legend = legend,
                           trim_char = "_", rank = "G")
  otu <- archi_plain_otu(ps)
  tax <- archi_plain_tax(ps)
  sam <- archi_plain_sam(ps)
  tree <- if (inherits(ps, "phyloseq")) phyloseq::phy_tree(ps) else ps$phy_tree
  expect_true(nrow(otu) > 5)
  expect_true(all(c("m11", "m12") %in% colnames(otu)))
  expect_true("genus" %in% names(tax))
  expect_true("stage" %in% names(sam))
  expect_true("profile_reads" %in% names(sam))
  expect_equal(sam["m11", "profile_reads"], 151064)
  expect_s3_class(tree, "phylo")
  expect_true(ape::Ntip(tree) >= 2)
  expect_setequal(tree$tip.label, rownames(otu))
  one <- read_classifier_report(
    list.files(path, pattern = "m11_.*k2", full.names = TRUE)[[1]],
    rank = "G"
  )
  expect_true(all(one$rank == "G"))
  expect_true(all(one$reads > 0))
  expect_false(anyNA(one$phylum))
  expect_false(any(grepl("^unclassified ", one$name)))
  expect_false(any(one$kingdom %in% c(
    "Terrabacteria group", "Bacteroidetes/Chlorobi group",
    "Cyanobacteria/Melainabacteria group", "PVC group"
  )))
})

test_that("real Bracken and KrakenUniq layouts reuse the same reader", {
  br <- system.file("extdata", "bracken-example.tsv", package = "aRchiteutis")
  back <- read_classifier_report(br, rank = "S")
  expect_equal(back$reads, c(368, 81, 62))
  expect_true(all(back$rank == "S"))
  expect_equal(attr(back, "profile_reads"), 511)

  uniq <- system.file("extdata", "krakenuniq-example.report", package = "aRchiteutis")
  got <- read_classifier_report(uniq, rank = "S")
  expect_equal(got$taxid, 694009L)
  expect_equal(got$genus, "Betacoronavirus")
  expect_equal(got$species, "Severe acute respiratory syndrome-related coronavirus")
  expect_equal(attr(got, "profile_reads"), 100)
})

test_that("Kraken2 minimizer columns are detected without accepting subranks", {
  src <- list.files(system.file("extdata", package = "aRchiteutis"),
                    pattern = "m11_.*k2", full.names = TRUE)[[1]]
  raw <- utils::read.table(src, sep = "\t", quote = "", fill = TRUE,
                          comment.char = "", stringsAsFactors = FALSE)
  raw <- utils::head(raw, 30)
  expanded <- data.frame(raw[, 1:3], minimizers = 1, distinct = 1,
                         rank = raw[, 4], taxid = raw[, 5],
                         name = raw[, 6], stringsAsFactors = FALSE)
  ufile <- tempfile(fileext = ".report")
  utils::write.table(expanded, ufile, sep = "\t", quote = FALSE,
                    row.names = FALSE, col.names = FALSE)
  got <- read_classifier_report(ufile, rank = "G")
  expect_true(nrow(got) >= 1)
  expect_true(all(got$taxid > 0))
  expect_false(any(grepl("^unclassified ", got$name)))
})

test_that("a QIIME 2 TSV manifest is preserved by Kraken import", {
  manifest <- tempfile(fileext = ".tsv")
  writeLines(c(
    "sample-id\ttarget",
    "#q2:types\tcategorical",
    "m11\tlarvae",
    "m12\tpupa"
  ), manifest)
  ps <- kraken_to_phyloseq(
    system.file("extdata", package = "aRchiteutis"),
    pattern = "m1[12]_.*k2", legend = manifest, trim_char = "_"
  )
  sam <- archi_plain_sam(ps)
  expect_equal(as.character(sam[c("m11", "m12"), "target"]), c("larvae", "pupa"))
})

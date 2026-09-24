test_that("NCBI efetch XML becomes a rank-formula tree", {
  xml <- paste(readLines(
    system.file("extdata", "ncbi_taxonomy.xml", package = "aRchiteutis"),
    warn = FALSE
  ), collapse = "\n")
  lin <- taxids_to_lineage(c(562L, 1578L), xml = xml)
  expect_equal(lin$tip_name, c("Escherichia coli", "Lactobacillus"))
  expect_equal(lin$genus, c("Escherichia", "Lactobacillus"))
  expect_false(anyNA(lin$phylum))
  tr <- taxids_to_tree(c(562L, 1578L), xml = xml)
  expect_s3_class(tr, "phylo")
  expect_true(all(c("Escherichia coli", "Lactobacillus") %in% tr$tip.label))
})

test_that("rank formula keeps a multifurcation and is not a binary hclust", {
  df <- data.frame(
    kingdom = c("Bacteria", "Bacteria", "Bacteria", "Bacteria", "Archaea"),
    phylum = c("Pseudomonadota", "Pseudomonadota", "Pseudomonadota",
               "Actinomycetota", "Methanobacteriota"),
    class = c("Gammaproteobacteria", "Gammaproteobacteria", "Gammaproteobacteria",
              "Actinomycetes", "Halobacteria"),
    order = c("Enterobacterales", "Enterobacterales", "Enterobacterales",
              "Mycobacteriales", "Halobacteriales"),
    family = c("Enterobacteriaceae", "Enterobacteriaceae", "Enterobacteriaceae",
               "Mycobacteriaceae", "Haloferacaceae"),
    genus = c("Escherichia", "Salmonella", "Klebsiella", "Mycobacterium", "Haloferax"),
    species = c("Escherichia coli", "Salmonella enterica", "Klebsiella pneumoniae",
                "Mycobacterium tuberculosis", "Haloferax volcanii"),
    stringsAsFactors = FALSE
  )
  tr <- ranks_to_tree(df)
  expect_equal(ape::Ntip(tr), 5L)
  kids <- table(tr$edge[, 1])
  expect_true(any(kids > 2L))
  expect_lt(max(ape::node.depth(tr)), 8)
})

test_that("sanitised rank labels map back to exact feature ids", {
  lineage <- data.frame(
    kingdom = "Bacteria", phylum = "Bacillota", class = "Bacilli",
    order = "Bacillales", family = "Bacillaceae",
    genus = c("Bacillus", "Bacillus"),
    species = c(
      "unclassified Bacillus (in: Bacteria)",
      "unclassified Bacillus [in: Bacteria]"
    ),
    tip_name = c("tax_1", "tax_2"),
    taxa_id = c("tax_1", "tax_2"),
    stringsAsFactors = FALSE
  )
  tree <- ranks_to_tree(lineage)
  expect_setequal(tree$tip.label, c("tax_1", "tax_2"))
})

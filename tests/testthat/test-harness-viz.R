test_that("rarefaction returns a curve and a replicate table", {
  sp <- archi_df()
  sp <- sp[sp$clade == "S", ]
  p <- df2rarefaction(sp, split_by = 7, depths = c(15L, 40L), n_reps = 2L, top = 20L)
  expect_builds(p)
  long <- attr(p, "rarefaction")
  expect_true(all(c("Sample", "Measure", "Depth", "value") %in% names(long)))
  expect_true(all(long$Depth %in% c(15L, 40L)))
  expect_true(all(is.finite(long$value)))
  expect_equal(max(archi_rarefaction_depths(c(120L, 350L))), 120L)
  mid <- archi_rarefaction_depths(c(2200L, 9000L))
  expect_equal(max(mid), 2200L)
  expect_true(all(mid <= 2200L))
})

test_that("differential tree uses a rank formula, not a binary hclust", {
  sp <- archi_df()
  sp <- sp[sp$clade == "S", ]
  p <- df2difftree(sp, group = "stage", max_tips = 8)
  expect_builds(p)
  tr <- archi_label_tree(c("Escherichia coli", "Escherichia fergusonii", "Bacillus subtilis"))
  expect_s3_class(tr, "phylo")
  expect_equal(ape::Ntip(tr), 3L)
  expect_lt(max(ape::node.depth(tr)), 20)
  expect_true(any(lengths(split(tr$edge[, 2], tr$edge[, 1])) > 2) ||
                ape::Ntip(tr) == 3)

  samples <- unique(as.character(sp$sample))
  groups <- stats::setNames(rep(c("a", "b", "c"), length.out = length(samples)), samples)
  sp$three_groups <- unname(groups[as.character(sp$sample)])
  expect_error(
    df2difftree(sp, group = "three_groups"),
    "more than two groups"
  )
  expect_builds(df2difftree(
    sp, group = "three_groups", contrast = c("a", "b"), max_tips = 8
  ))
  expect_builds(df2difftree(sp, group = "stage", max_tips = 8, fruit = "bar"))
})

test_that("difftree heatmap uses gheatmap on a seven-rank tax tree", {
  skip_if_not_installed("ggtree")
  df <- archi_df()
  sp <- df[df$clade == "S", ]
  taxa <- unique(as.character(sp$taxa))
  taxa <- taxa[seq_len(min(8L, length(taxa)))]
  sp <- sp[sp$taxa %in% taxa, ]
  tax <- data.frame(
    taxa = taxa,
    kingdom = "Bacteria",
    phylum = rep(c("Bacillota", "Pseudomonadota"), length.out = length(taxa)),
    class = rep(c("Bacilli", "Gammaproteobacteria"), length.out = length(taxa)),
    order = rep(c("Lactobacillales", "Enterobacterales"), length.out = length(taxa)),
    family = taxa,
    genus = taxa,
    species = taxa,
    stringsAsFactors = FALSE
  )
  tr <- archi_taxa_tree(taxa, tax)
  expect_gte(max(ape::node.depth.edgelength(tr)), 6)
  p <- df2difftree(sp, group = "stage", max_tips = 8, tax = tax)
  expect_builds(p)
  geoms <- vapply(p$layers, function(layer) class(layer$geom)[[1]], character(1))
  expect_true(any(geoms %in% c("GeomTile", "GeomRect")))
})

test_that("heat tree and upset plots build on the bundled reports", {
  g <- archi_df()
  g <- g[g$clade == "G", ]
  skip_if_not_installed("metacoder")
  expect_builds(df2heattree(g, top = 10))
  skip_if_not_installed("ComplexUpset")
  p <- df2upset(g, group = "stage", min_size = 1)
  expect_s3_class(p, "ggplot")
  expect_no_error(withr::with_pdf(tempfile(fileext = ".pdf"), print(p)))
})

test_that("microbiota difftree builds a ggtree on the bundled reports", {
  skip_if_not_installed("MicrobiotaProcess")
  skip_if_not_installed("ggstar")
  skip_if_not_installed("phyloseq")
  df <- get_counts(
    extdata_path(),
    pattern = "m(11|12|13|18|4|39)_",
    legend = file.path(extdata_path(), "legend.csv"),
    trim_char = "_"
  )
  g <- df[df$clade == "G", ]
  p <- df2difftree(
    g, group = "stage", contrast = c("larvae", "pupa"),
    engine = "microbiota", max_tips = 30
  )
  expect_s3_class(p, "ggtree")
  expect_builds(p)
})

test_that("microbiota difftree stops without MicrobiotaProcess", {
  skip_if(requireNamespace("MicrobiotaProcess", quietly = TRUE))
  g <- archi_df()
  g <- g[g$clade == "G", ]
  expect_error(
    df2difftree(g, group = "stage", engine = "microbiota"),
    "MicrobiotaProcess"
  )
})

test_that("heat tree and metacoder difftree stop without metacoder", {
  skip_if(requireNamespace("metacoder", quietly = TRUE))
  g <- archi_df()
  g <- g[g$clade == "G", ]
  expect_error(df2heattree(g, top = 10), "metacoder")
  expect_error(
    df2difftree(g, group = "stage", max_tips = 8, engine = "metacoder"),
    "metacoder"
  )
})

test_that("upset stops without ComplexUpset", {
  skip_if(requireNamespace("ComplexUpset", quietly = TRUE))
  g <- archi_df()
  g <- g[g$clade == "G", ]
  expect_error(df2upset(g, group = "stage"), "ComplexUpset")
})

test_that("metacoder heat tree receives a taxon_id abundance table", {
  skip_if_not_installed("metacoder")
  g <- archi_df()
  g <- g[g$clade == "G", ]
  taxa <- unique(as.character(g$taxa))[seq_len(5)]
  tax <- data.frame(
    taxa = taxa,
    kingdom = "Bacteria",
    phylum = "Bacillota",
    genus = taxa,
    stringsAsFactors = FALSE
  )
  expect_no_error(df2heattree(g[g$taxa %in% taxa, ], tax = tax))
})

test_that("metacoder taxonomy is one connected tree", {
  skip_if_not_installed("metacoder")
  g <- archi_df()
  g <- g[g$clade == "G", ]
  taxa <- unique(as.character(g$taxa))[seq_len(8)]
  g <- g[g$taxa %in% taxa, ]
  tax <- data.frame(
    taxa = taxa,
    kingdom = "Bacteria",
    phylum = rep(c("Firmicutes", "Proteobacteria"), length.out = 8),
    class = c("Bacilli", NA, "Gammaproteobacteria", NA, "Bacilli",
              "Gammaproteobacteria", "Bacilli", NA),
    genus = taxa,
    species = NA_character_,
    stringsAsFactors = FALSE
  )
  obj <- aRchiteutis:::archi_taxmap_abundance(g, tax)
  expect_silent(aRchiteutis:::archi_assert_taxmap_tree(obj))
  expect_false(any(obj$taxon_names() == "NA", na.rm = TRUE))
  expect_false("root" %in% obj$taxon_names())

  tax$kingdom[seq_len(4)] <- "Archaea"
  two <- aRchiteutis:::archi_taxmap_abundance(g, tax)
  expect_silent(aRchiteutis:::archi_assert_taxmap_tree(two))
  expect_true("root" %in% two$taxon_names())

  bare <- aRchiteutis:::archi_taxmap_abundance(g, NULL)
  expect_silent(aRchiteutis:::archi_assert_taxmap_tree(bare))
  expect_length(bare$roots(), 1L)

  ps <- kraken_to_phyloseq(
    system.file("extdata", package = "aRchiteutis"),
    pattern = "m(11|18)_",
    legend = system.file("extdata", "legend.csv", package = "aRchiteutis"),
    trim_char = "_", rank = "G"
  )
  converted <- phyloseq_to_metacoder(ps)
  expect_silent(aRchiteutis:::archi_assert_taxmap_tree(converted))
  expect_equal(sum(converted$n_supertaxa() == 0), 1)
  expect_equal(unname(converted$taxon_names()[converted$roots()]), "Bacteria")

  ps_s <- kraken_to_phyloseq(
    system.file("extdata", package = "aRchiteutis"),
    pattern = "m(11|18)_",
    legend = system.file("extdata", "legend.csv", package = "aRchiteutis"),
    trim_char = "_", rank = "S"
  )
  species <- phyloseq_to_metacoder(ps_s)
  expect_silent(aRchiteutis:::archi_assert_taxmap_tree(species))
  expect_equal(unname(species$taxon_names()[species$roots()]), "Bacteria")
  expect_false(any(species$taxon_names() == "NA", na.rm = TRUE))
})

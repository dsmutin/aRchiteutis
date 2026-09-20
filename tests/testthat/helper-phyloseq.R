# Fixtures for the phyloseq-compatibility tests.
#
# We build ONE small, fully-specified phyloseq object (constructed directly with
# phyloseq::phyloseq()) and reuse it across the visualisation tests so they stay
# fast and deterministic. A second fixture built from the shipped example data
# via df2ps() is used for the round-trip correctness checks.

# Small synthetic phyloseq object: 6 samples (3 "pupa", 3 "larvae"), a full
# Domain..Species lineage and two sample-data variables (stage, hive). Enough
# taxa for the matrix-oriented plots (PCA, corrplot, chord, t-SNE) to behave.
.build_archi_ps <- function() {
  set.seed(20240501)

  phylum <- c("Firmicutes", "Proteobacteria")
  class  <- c("Bacilli", "Gammaproteobacteria")
  order  <- c("Lactobacillales", "Bacillales", "Enterobacterales")
  family <- c("Lactobacillaceae", "Bacillaceae",
              "Enterobacteriaceae", "Erwiniaceae")

  # 12 genera, 2 species each -> 24 species (operational taxa).
  genera <- c("Apilactobacillus", "Lactobacillus", "Fructobacillus",
              "Bacillus", "Paenibacillus", "Lysinibacillus",
              "Escherichia", "Klebsiella", "Enterobacter",
              "Erwinia", "Pantoea", "Tatumella")
  gen_family <- c(rep("Lactobacillaceae", 3), rep("Bacillaceae", 3),
                  rep("Enterobacteriaceae", 3), rep("Erwiniaceae", 3))
  gen_order <- c(rep("Lactobacillales", 3), rep("Bacillales", 3),
                 rep("Enterobacterales", 6))
  gen_class <- c(rep("Bacilli", 6), rep("Gammaproteobacteria", 6))
  gen_phylum <- c(rep("Firmicutes", 6), rep("Proteobacteria", 6))

  species <- paste(rep(genera, each = 2),
                   rep(c("alpha", "beta"), times = length(genera)))

  tax <- cbind(
    Domain  = "Bacteria",
    Phylum  = rep(gen_phylum, each = 2),
    Class   = rep(gen_class, each = 2),
    Order   = rep(gen_order, each = 2),
    Family  = rep(gen_family, each = 2),
    Genus   = rep(genera, each = 2),
    Species = species)
  rownames(tax) <- species

  samples <- c("m1_pupa", "m2_pupa", "m3_pupa",
               "m4_larvae", "m5_larvae", "m6_larvae")

  otu <- matrix(stats::rpois(length(species) * length(samples), lambda = 40),
                nrow = length(species), ncol = length(samples),
                dimnames = list(species, samples))
  # Inject a group difference so the volcano / ordination plots have signal.
  otu[1:6, 1:3] <- otu[1:6, 1:3] + 60
  otu[7:12, 4:6] <- otu[7:12, 4:6] + 60

  sdata <- data.frame(
    stage = c("pupa", "pupa", "pupa", "larvae", "larvae", "larvae"),
    hive  = c(1L, 1L, 2L, 1L, 2L, 2L),
    row.names = samples)

  phyloseq::phyloseq(
    phyloseq::otu_table(otu, taxa_are_rows = TRUE),
    phyloseq::tax_table(tax),
    phyloseq::sample_data(sdata))
}

# Cache the fixture once (only when phyloseq is available).
.archi_ps <- NULL
if (requireNamespace("phyloseq", quietly = TRUE)) {
  suppressMessages(suppressWarnings({
    .archi_ps <- .build_archi_ps()
  }))
}

# Accessor used by the tests; skips the whole test when phyloseq is absent.
archi_ps <- function() {
  testthat::skip_if_not_installed("phyloseq")
  .archi_ps
}

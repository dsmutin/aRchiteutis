# phyloseq compatibility: ps2df() / df2ps() and every df2* visualisation driven
# directly from a phyloseq object. All tests are skipped when phyloseq is not
# installed (it is a heavy Bioconductor Suggests dependency).

skip_if_not_installed("phyloseq")

# ---------------------------------------------------------------------------
# ps2df(): shape, positions, clade letters, amounts
# ---------------------------------------------------------------------------

test_that("ps2df returns the documented column layout and positions", {
  d <- ps2df(archi_ps())

  expect_s3_class(d, "tbl_df")
  # Fixed columns 1-6, in order.
  expect_identical(names(d)[1:6],
                   c("taxa", "clade", "sample", "N", "amount", "amount_cl"))
  # Metadata / legend columns follow at 7+.
  expect_identical(names(d)[7:8], c("stage", "hive"))

  expect_s3_class(d$taxa, "factor")
  expect_s3_class(d$clade, "factor")
  expect_s3_class(d$sample, "factor")
  expect_type(d$N, "double")
})

test_that("ps2df emits the expected clade letters incl. a root row", {
  d <- ps2df(archi_ps())
  clades <- sort(unique(as.character(d$clade)))
  # Full Domain..Species lineage plus a root ("R") row.
  expect_true(all(c("D", "P", "C", "O", "F", "G", "S", "R") %in% clades))
  expect_true(all(d$taxa[d$clade == "R"] == "root"))
})

test_that("ps2df amounts lie in [0, 1] and classified == total", {
  d <- ps2df(archi_ps())
  expect_true(all(d$amount >= 0 & d$amount <= 1))
  expect_true(all(d$amount_cl >= 0 & d$amount_cl <= 1))
  # No unclassified info in a plain phyloseq -> the two fractions coincide.
  expect_equal(d$amount, d$amount_cl)
  # Root rows are the per-sample total, so their fraction is 1.
  expect_equal(unname(d$amount[d$clade == "R"]),
               rep(1, sum(d$clade == "R")))
})

test_that("ps2df errors clearly on a non-phyloseq input", {
  expect_error(ps2df(mtcars), "phyloseq")
})

# ---------------------------------------------------------------------------
# df2ps() and the round trip
# ---------------------------------------------------------------------------

test_that("df2ps builds a phyloseq object with a full lineage", {
  ps <- suppressMessages(suppressWarnings(df2ps(archi_df())))
  expect_s4_class(ps, "phyloseq")
  expect_true(all(c("Domain", "Genus", "Species") %in%
                    phyloseq::rank_names(ps)))
  expect_equal(phyloseq::nsamples(ps), nlevels(droplevels(archi_df()$sample)))
})

test_that("df2ps -> ps2df preserves genus-level counts", {
  df <- archi_df()

  ps_g <- suppressMessages(suppressWarnings(df2ps(df, clade = "G")))
  back <- ps2df(ps_g)

  # Original (classified) genus counts per sample.
  orig <- suppressMessages(dplyr::summarise(
    df[df$clade == "G" & !grepl("unclassified", df$taxa), ],
    N = sum(N), .by = c("taxa", "sample")))
  rt <- suppressMessages(dplyr::summarise(
    back[back$clade == "G", ], N = sum(N), .by = c("taxa", "sample")))

  orig$key <- paste(orig$taxa, orig$sample)
  rt$key <- paste(rt$taxa, rt$sample)

  # Every original genus/sample count survives the round trip unchanged.
  expect_true(all(orig$key %in% rt$key))
  m <- merge(orig, rt, by = "key", suffixes = c(".o", ".r"))
  expect_identical(m$N.o, m$N.r)
})

# ---------------------------------------------------------------------------
# Every visualisation function, called DIRECTLY on the phyloseq object.
# ---------------------------------------------------------------------------

# --- composition plots -----------------------------------------------------

test_that("df2donut works on a phyloseq object", {
  expect_builds(suppressMessages(suppressWarnings(df2donut(archi_ps()))))
})

test_that("df2composition works on a phyloseq object", {
  expect_builds(suppressMessages(suppressWarnings(df2composition(archi_ps()))))
})

test_that("df2barplot works on a phyloseq object", {
  expect_builds(suppressMessages(suppressWarnings(df2barplot(archi_ps()))))
})

# --- diversity plots -------------------------------------------------------

test_that("df2alpha works on a phyloseq object", {
  expect_builds(suppressMessages(suppressWarnings(
    df2alpha(archi_ps(), add_legend = 7))))
})

test_that("df2alpha_summary works on a phyloseq object", {
  expect_builds(suppressMessages(suppressWarnings(
    df2alpha_summary(archi_ps(), split_by = 7, add_legend = 7:8))))
})

test_that("df2beta draws and returns a distance matrix from a phyloseq object", {
  d <- suppressMessages(df2beta(archi_ps(), print_df = TRUE))
  expect_true(is.matrix(d))
  expect_equal(nrow(d), ncol(d))
  expect_equal(nrow(d), phyloseq::nsamples(archi_ps()))
  expect_draws(suppressMessages(suppressWarnings(
    df2beta(archi_ps(), add_legend = 7:8, add_labels = 7))))
})

test_that("df2beta_bray works on a phyloseq object", {
  expect_draws(suppressMessages(suppressWarnings(df2beta_bray(archi_ps()))))
})

test_that("df2beta_pcoa works on a phyloseq object", {
  expect_builds(suppressMessages(suppressWarnings(
    df2beta_pcoa(archi_ps(), add_legend = 7))))
})

# --- ordination / matrix plots ---------------------------------------------

test_that("df2pca_sample works on a phyloseq object", {
  expect_builds(suppressMessages(suppressWarnings(df2pca_sample(archi_ps()))))
})

test_that("df2pca_sp works on a phyloseq object", {
  expect_builds(suppressMessages(suppressWarnings(df2pca_sp(archi_ps()))))
})

test_that("df2heatmap works on a phyloseq object", {
  expect_draws(suppressMessages(suppressWarnings(
    df2heatmap(archi_ps(), scale = "row", Colv = NA))))
})

test_that("df2cluster works on a phyloseq object", {
  expect_draws(suppressMessages(suppressWarnings(
    df2cluster(archi_ps(), k_means = 3, use = "sp"))))
})

test_that("df2clust2d works on a phyloseq object", {
  set.seed(1)
  expect_builds(suppressMessages(suppressWarnings(
    df2clust2d(archi_ps(), legend_detect = "pupa", top = 12, k_means = 3))))
})

test_that("df2corrplot works on a phyloseq object", {
  expect_draws(suppressMessages(suppressWarnings(
    df2corrplot(archi_ps(), k_means = 3))))
})

test_that("df2chord works on a phyloseq object", {
  set.seed(1)
  expect_builds(suppressMessages(suppressWarnings(
    df2chord(archi_ps(), k_means = 3, coenf_level = 0.5))))
})

test_that("df2tsne works on a phyloseq object", {
  set.seed(1)
  expect_builds(suppressMessages(suppressWarnings(
    df2tsne(archi_ps(), k_means = 3, perplexity = 5, max_iter = 100))))
})

test_that("df2volcano works on a phyloseq object", {
  expect_builds(suppressMessages(suppressWarnings(
    df2volcano(archi_ps(), legend_detect = c("pupa", "larvae")))))
})

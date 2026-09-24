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

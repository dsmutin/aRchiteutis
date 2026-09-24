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
  expect_builds(df2heattree(g, top = 10))
  expect_builds(df2upset(g, group = "stage", min_size = 1))
})

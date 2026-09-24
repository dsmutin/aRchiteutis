# Beta-diversity distances adapted from the metagenomics harness:
# phyloseq::distance methods (via vegan::vegdist when phyloseq is absent)
# and robCompositions::aDist after compositional zero replacement.

#' Martín-Fernández et al. (2003) multiplicative replacement
#' @keywords internal
archi_coda_mult_repl <- function(X, frac = 0.65) {
  rs <- rowSums(X)
  if (any(rs <= 0)) {
    stop("Zero-sum sample in Aitchison zero replacement: ",
         paste(rownames(X)[rs <= 0], collapse = ","), call. = FALSE)
  }
  Xc <- sweep(X, 1, rs, "/")
  all_pos <- Xc[Xc > 0]
  if (!length(all_pos)) stop("No positive values for Aitchison zero replacement", call. = FALSE)
  gmin <- min(all_pos)
  dl <- apply(Xc, 2, function(v) {
    pos <- v[v > 0]
    if (length(pos)) min(pos) else gmin
  })
  delta <- frac * dl
  for (i in seq_len(nrow(Xc))) {
    z <- Xc[i, ] == 0
    if (!any(z)) next
    repl <- delta[z]
    s <- sum(repl)
    if (!(s < 1)) {
      stop("Zero-replacement mass >= 1 in sample ", rownames(Xc)[i], call. = FALSE)
    }
    Xc[i, z] <- repl
    Xc[i, !z] <- Xc[i, !z] * (1 - s)
  }
  Xc
}

#' Replace zeros before Aitchison distance
#' @keywords internal
archi_replace_zeros_coda <- function(X) {
  n_zero <- as.integer(sum(X == 0))
  if (!n_zero) {
    return(list(X = sweep(X, 1, rowSums(X), "/"), method = "none", n_zero = 0L))
  }
  if (requireNamespace("robCompositions", quietly = TRUE) && ncol(X) <= 80L) {
    Xc <- sweep(X, 1, rowSums(X), "/")
    dl <- apply(Xc, 2, function(v) {
      pos <- v[v > 0]
      if (length(pos)) 0.65 * min(pos) else 0
    })
    dl[colSums(Xc == 0) == 0] <- 0
    rz <- tryCatch(
      robCompositions::impRZilr(Xc, dl = dl, method = "lm", nComp = 3, verbose = FALSE),
      error = function(e) e
    )
    if (!inherits(rz, "error") && !is.null(rz$x)) {
      Ximp <- as.matrix(rz$x)
      dimnames(Ximp) <- dimnames(X)
      return(list(X = Ximp, method = "robCompositions::impRZilr", n_zero = n_zero))
    }
  }
  list(X = archi_coda_mult_repl(X, frac = 0.65),
       method = "multRepl_Martin-Fernandez_2003", n_zero = n_zero)
}

#' Aitchison distance via robCompositions::aDist
#' @param X Numeric matrix, samples in rows and taxa in columns.
#' @keywords internal
archi_aitchison_distance <- function(X) {
  if (!requireNamespace("robCompositions", quietly = TRUE)) {
    stop("Aitchison distance needs robCompositions::aDist", call. = FALSE)
  }
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  if (any(!is.finite(X)) || any(X < 0)) {
    stop("OTU table has negative or non-finite values", call. = FALSE)
  }
  keep <- colSums(X) > 0
  if (any(!keep)) X <- X[, keep, drop = FALSE]
  if (ncol(X) < 2L) stop("Aitchison distance needs at least two taxa", call. = FALSE)
  if (any(rowSums(X) <= 0)) stop("Zero-sum sample after dropping empty taxa", call. = FALSE)
  zr <- archi_replace_zeros_coda(X)
  if (any(zr$X <= 0) || any(!is.finite(zr$X))) {
    stop("Aitchison zero replacement left non-positive values", call. = FALSE)
  }
  d <- robCompositions::aDist(zr$X)
  out <- as.matrix(d)
  dimnames(out) <- list(rownames(X), rownames(X))
  attr(out, "zero_method") <- zr$method
  out
}

#' Canonical beta-diversity method names
#'
#' Includes the phyloseq distance set (Bray, Jaccard, UniFrac, weighted
#' UniFrac, Jensen-Shannon, DPCoA) plus the vegan distances and Aitchison
#' (`robCompositions::aDist`).
#'
#' @return Character vector of method ids.
#' @export
archi_beta_methods <- function() {
  c("bray", "jaccard", "aitchison", "unifrac", "wunifrac", "jsd", "dpcoa",
    "manhattan", "euclidean", "canberra", "kulczynski", "gower", "altGower",
    "morisita", "horn", "mountford", "raup", "binomial", "chao", "cao")
}

archi_normalize_distance_name <- function(method) {
  method <- tolower(trimws(method))
  aliases <- c(
    bray = "bray", `bray-curtis` = "bray", `bray_curtis` = "bray",
    jaccard = "jaccard",
    aitchison = "aitchison", clr = "aitchison",
    unifrac = "unifrac",
    wunifrac = "wunifrac", `weighted-unifrac` = "wunifrac",
    `weighted_unifrac` = "wunifrac",
    jsd = "jsd", dpcoa = "dpcoa"
  )
  if (method %in% names(aliases)) return(unname(aliases[[method]]))
  if (!method %in% archi_beta_methods()) {
    stop("Unknown distance: ", method,
         ". See archi_beta_methods().", call. = FALSE)
  }
  method
}

#' Pairwise beta-diversity matrix for a tidy table
#'
#' @param df Tidy table from [get_counts()].
#' @param method Distance id from [archi_beta_methods()].
#' @param tree Optional `phylo` required for UniFrac methods. When missing,
#'   a genus/species formula tree is built from taxon labels.
#' @return Symmetric numeric matrix with sample names on both margins.
#' @export
archi_distance_matrix <- function(df, method = "bray", tree = NULL) {
  method <- archi_normalize_distance_name(method)
  counts <- df_untidy(df, amount_from = "N", drop_unclassified = TRUE)
  X <- t(counts)
  if (identical(method, "aitchison")) return(archi_aitchison_distance(X))
  phy <- c("unifrac", "wunifrac", "jsd", "dpcoa")
  if (method %in% phy) {
    return(archi_phyloseq_distance(counts, method, tree))
  }
  if (!requireNamespace("vegan", quietly = TRUE)) {
    if (identical(method, "bray")) return(df_beta_matrix(df, abdiv::bray_curtis))
    stop("Distance '", method, "' needs the vegan package", call. = FALSE)
  }
  # phyloseq delegates these methods to vegan on the count table. In
  # particular Chao, Cao and Morisita require integer-like counts; converting
  # every sample to proportions makes those distances meaningless.
  d <- vegan::vegdist(X, method = method)
  out <- as.matrix(d)
  dimnames(out) <- list(rownames(X), rownames(X))
  out
}

#' phyloseq::distance for UniFrac, JSD and DPCoA
#' @keywords internal
archi_phyloseq_distance <- function(counts, method, tree = NULL) {
  if (!requireNamespace("phyloseq", quietly = TRUE)) {
    stop("Distance '", method, "' needs phyloseq", call. = FALSE)
  }
  if (is.null(tree)) tree <- archi_label_tree(rownames(counts))
  tree <- ape::keep.tip(tree, intersect(tree$tip.label, rownames(counts)))
  counts <- counts[intersect(rownames(counts), tree$tip.label), , drop = FALSE]
  otu <- phyloseq::otu_table(counts, taxa_are_rows = TRUE)
  ps <- phyloseq::phyloseq(otu, phyloseq::phy_tree(tree))
  d <- phyloseq::distance(ps, method = method)
  as.matrix(d)
}

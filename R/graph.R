# tidygraph -> ggraph engine for taxon co-occurrence networks.
#
# `df2chord()` (in plots_ordination.R) stays the circular chord view built on
# base igraph. The functions here are a tidy, general-purpose alternative: they
# turn a taxa-by-sample matrix into a `tidygraph::tbl_graph` annotated with node
# centralities and community membership (the CALC step, `df2graph()`), and draw
# it with ggraph using a force-directed / user-chosen layout (the VIZ step,
# `df2ggraph()`). Both accept the same df / phyloseq / matrix inputs as the rest
# of the package via `as_samovar_matrix()`.

# Community-detection dispatch: map a friendly name to the tidygraph grouping
# verb applied inside `dplyr::mutate(activate(nodes))`. Every method operates on
# the (undirected) graph; weights are the absolute correlations.
.archi_group_call <- function(cluster_method) {
  cluster_method <- match.arg(
    cluster_method,
    c("louvain", "walktrap", "infomap", "label_prop", "fast_greedy",
      "leading_eigen", "edge_betweenness", "components"))
  switch(
    cluster_method,
    louvain          = tidygraph::group_louvain(weights = .data$weight),
    walktrap         = tidygraph::group_walktrap(weights = .data$weight),
    infomap          = tidygraph::group_infomap(weights = .data$weight),
    label_prop       = tidygraph::group_label_prop(weights = .data$weight),
    fast_greedy      = tidygraph::group_fast_greedy(weights = .data$weight),
    leading_eigen    = tidygraph::group_leading_eigen(weights = .data$weight),
    edge_betweenness = tidygraph::group_edge_betweenness(weights = .data$weight),
    components       = tidygraph::group_components())
}

# Build the thresholded, annotated tbl_graph from a correlation matrix. Shared
# by df2graph() and the NetCoMi bridge (netcomi.R) so both produce identically
# structured graphs for df2ggraph().
.archi_cormat_to_tbl_graph <- function(cmat, threshold = 0.3,
                                       cluster_method = "louvain") {
  cmat[is.na(cmat)] <- 0
  diag(cmat) <- 0

  # Upper-triangle edge list of taxon pairs passing the |cor| threshold.
  nm <- rownames(cmat)
  if (is.null(nm)) nm <- as.character(seq_len(nrow(cmat)))
  ut <- which(upper.tri(cmat), arr.ind = TRUE)
  keep <- abs(cmat[ut]) >= threshold
  edges <- data.frame(
    from = nm[ut[keep, 1]],
    to   = nm[ut[keep, 2]],
    weight = as.numeric(cmat[ut][keep]),
    stringsAsFactors = FALSE)
  edges$abs_weight <- abs(edges$weight)
  edges$sign <- ifelse(edges$weight >= 0, "positive", "negative")

  nodes <- data.frame(name = nm, stringsAsFactors = FALSE)

  g <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

  # Node- and edge-level annotations via tidygraph verbs. Absolute correlation
  # is used as the edge weight for weighted centralities / communities.
  g <- g %>%
    tidygraph::activate("edges") %>%
    dplyr::mutate(weight = abs(.data$weight)) %>%
    tidygraph::activate("nodes") %>%
    dplyr::mutate(
      degree      = tidygraph::centrality_degree(weights = .data$weight,
                                                 loops = FALSE),
      betweenness = tidygraph::centrality_betweenness(weights = .data$weight,
                                                      directed = FALSE),
      closeness   = suppressWarnings(
        tidygraph::centrality_closeness(weights = .data$weight)),
      community   = as.factor(.archi_group_call(cluster_method)))

  # Restore the signed correlation on the edges (the weighted-centrality step
  # above overwrote `weight` with its absolute value for the calculations).
  g <- g %>%
    tidygraph::activate("edges") %>%
    dplyr::mutate(
      weight     = .data$abs_weight * ifelse(.data$sign == "negative", -1, 1),
      abs_weight = .data$abs_weight)

  tidygraph::activate(g, "nodes")
}

#' Build a tidygraph co-occurrence network from taxa
#'
#' Computes a taxon-by-taxon correlation matrix, keeps edges whose absolute
#' correlation is at least `threshold`, and returns a
#' [tidygraph::tbl_graph]. Nodes are annotated with weighted centralities
#' (degree, betweenness, closeness) and community membership; edges carry the
#' signed correlation (`weight`), its magnitude (`abs_weight`) and `sign`.
#'
#' This is the CALC half of the tidygraph / ggraph engine; pass the result to
#' [df2ggraph()] to draw it. [df2chord()] remains the separate circular chord
#' view.
#'
#' @param df A numeric taxa-by-sample matrix, e.g. from [df_untidy()]. A
#'   [phyloseq::phyloseq] object or a long `df` from [get_counts()] is also
#'   accepted and reduced to a matrix first with [df_untidy()] (a plain matrix
#'   is used as-is).
#' @param clade Character. Clade letter used when reducing a `df` / phyloseq
#'   object to a matrix (default `"G"`, genus).
#' @param cor_method Correlation method, one of `"pearson"`, `"spearman"` or
#'   `"kendall"` (passed to [stats::cor]).
#' @param threshold Numeric in `[0, 1]`. Keep only edges with
#'   `abs(correlation) >= threshold`.
#' @param cluster_method Community-detection method: one of `"louvain"`,
#'   `"walktrap"`, `"infomap"`, `"label_prop"`, `"fast_greedy"`,
#'   `"leading_eigen"`, `"edge_betweenness"` or `"components"`.
#' @param top Integer or `FALSE`. Keep only the `top` most abundant taxa when
#'   reducing a `df` / phyloseq object (ignored for a bare matrix).
#' @param ... Passed to [as_samovar_matrix()].
#'
#' @return A [tidygraph::tbl_graph] with the nodes context active.
#'
#' @seealso [df2ggraph()] to visualise the graph, [df2chord()] for the chord
#'   view.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[13][124]_", trim_char = "_")
#' mat <- df_untidy(df, clade = "G", top = 12, scale = "scale")
#' set.seed(1)
#' g <- df2graph(mat, threshold = 0.3)
#' g
#'
#' @export
#' @importFrom rlang .data
df2graph <- function(df, clade = "G", cor_method = "pearson", threshold = 0.3,
                     cluster_method = "louvain", top = FALSE, ...) {
  mat <- as_samovar_matrix(df, clade = clade, top = top, scale = FALSE, ...)
  mat <- as.matrix(mat)
  cmat <- stats::cor(t(mat), method = cor_method)
  .archi_cormat_to_tbl_graph(cmat, threshold = threshold,
                             cluster_method = cluster_method)
}

#' Visualise a tidygraph network with ggraph
#'
#' Draws a [tidygraph::tbl_graph] with \pkg{ggraph}: edges coloured by the
#' signed correlation (red = negative, blue = positive) with alpha scaled by
#' magnitude, nodes coloured by community (viridis) and sized by centrality, and
#' taxon labels in italic. This is the VIZ half of the engine; when given a
#' `df` / phyloseq / matrix it calls [df2graph()] first.
#'
#' @param x A [tidygraph::tbl_graph] (e.g. from [df2graph()]), or a `df` /
#'   [phyloseq::phyloseq] object / matrix, in which case [df2graph()] is run
#'   first.
#' @param layout A \pkg{ggraph} layout name (default `"fr"`, Fruchterman-Reingold).
#' @param size_by Node column mapped to point size (default `"degree"`).
#' @param label Logical. Draw taxon labels.
#' @param label_size Numeric. Label text size.
#' @param node_alpha Numeric. Node point alpha.
#' @param ... Passed to [df2graph()] when `x` is not already a `tbl_graph`.
#'
#' @return A [ggplot2::ggplot] object (a \pkg{ggraph} plot).
#'
#' @seealso [df2graph()] for the graph construction.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[13][124]_", trim_char = "_")
#' mat <- df_untidy(df, clade = "G", top = 12, scale = "scale")
#' set.seed(1)
#' df2ggraph(mat, threshold = 0.3)
#'
#' @export
#' @importFrom rlang .data
df2ggraph <- function(x, layout = "fr", size_by = "degree", label = TRUE,
                      label_size = 3, node_alpha = 0.9, ...) {
  if (!inherits(x, "tbl_graph")) x <- df2graph(x, ...)

  g <- tidygraph::activate(x, "nodes")
  n_comm <- length(unique(igraph::V(g)$community))
  n_comm <- max(n_comm, 1L)

  # Force-directed layouts (fr, kk, ...) read the `weight` edge attribute, which
  # here is a *signed* correlation; feed them the magnitude instead so negative
  # correlations do not break the layout. Fall back to an unweighted layout for
  # layouts that do not accept weights.
  lay <- tryCatch(
    ggraph::create_layout(g, layout = layout, weights = .data$abs_weight),
    error = function(e) ggraph::create_layout(g, layout = layout))

  p <- ggraph::ggraph(lay) +
    ggraph::geom_edge_link(
      ggplot2::aes(edge_colour = .data$weight,
                   edge_alpha = .data$abs_weight),
      edge_width = 0.6, show.legend = TRUE) +
    ggraph::geom_node_point(
      ggplot2::aes(colour = .data$community, size = .data[[size_by]]),
      alpha = node_alpha) +
    ggraph::scale_edge_colour_gradient2(
      "correlation", low = "#B2182B", mid = "grey90", high = "#2166AC",
      midpoint = 0, limits = c(-1, 1), guide = "none") +
    ggraph::scale_edge_alpha_continuous("|correlation|", range = c(0.1, 0.9),
                                        guide = "none") +
    ggplot2::scale_colour_manual("community",
                                 values = viridis::viridis(n_comm)) +
    ggplot2::scale_size_continuous(size_by, range = c(2, 8)) +
    ggplot2::coord_fixed() +
    ggraph::theme_graph(base_family = "sans")

  if (isTRUE(label)) {
    p <- p + ggraph::geom_node_text(
      ggplot2::aes(label = .data$name), repel = TRUE, fontface = "italic",
      size = label_size, show.legend = FALSE)
  }

  p
}

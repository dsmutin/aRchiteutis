#' PCA of samples
#'
#' Runs a principal component analysis treating samples as observations and taxa
#' as variables, and draws the individuals plot with \pkg{factoextra}.
#'
#' @param df A numeric taxa-by-sample matrix, e.g. from [df_untidy()].
#' @param scale Logical. Scale variables to unit variance before PCA.
#' @param detect Character or `FALSE`. If set, samples whose column name matches
#'   this pattern are coloured with `detect`, the rest with `detect2`.
#' @param detect2 Character. Colour label for non-matching samples.
#' @param ... Passed to [factoextra::fviz_pca_ind].
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[13][124]_", trim_char = "_")
#' mat <- df_untidy(df, clade = "S", scale = "scale", keep_sample_name = FALSE)
#' df2pca_sample(mat, scale = FALSE)
#'
#' @export
df2pca_sample <- function(df, scale = TRUE, detect = FALSE,
                          detect2 = "other", ...) {
  mat <- df
  if (isTRUE(scale)) mat <- mat[apply(mat, 1, stats::var) > 0, , drop = FALSE]

  res_pca <- stats::prcomp(t(mat), scale. = scale)

  if (!isFALSE(detect)) {
    col_list <- stringr::str_detect(colnames(mat), detect)
    color_list <- ifelse(col_list, detect, detect2)
  } else {
    color_list <- rep("sample", ncol(mat))
  }

  factoextra::fviz_pca_ind(res_pca, col.ind = color_list, ...) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle("")
}

#' PCA of taxa
#'
#' Like [df2pca_sample()] but draws the variables (taxa) plot.
#'
#' @inheritParams df2pca_sample
#' @param ... Passed to [factoextra::fviz_pca_var].
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[13][124]_", trim_char = "_")
#' df2pca_sp(df_untidy(df, clade = "G", top = 10), scale = TRUE)
#'
#' @export
df2pca_sp <- function(df, scale = TRUE, ...) {
  mat <- df
  if (isTRUE(scale)) mat <- mat[apply(mat, 1, stats::var) > 0, , drop = FALSE]

  res_pca <- stats::prcomp(t(mat), scale. = scale)

  factoextra::fviz_pca_var(res_pca, ...) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle("")
}

#' Base-R heatmap of a taxa-by-sample matrix
#'
#' @param df A numeric taxa-by-sample matrix, e.g. from [df_untidy()].
#' @param clade Ignored; kept for backward compatibility (do the clade / trim
#'   selection in [df_untidy()] instead).
#' @param trim Ignored; kept for backward compatibility.
#' @param ... Passed to [stats::heatmap].
#'
#' @return Invisibly, the result of [stats::heatmap] (drawn as a side effect).
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[13][124]_", trim_char = "_")
#' mat <- df_untidy(df, clade = "G", top = 10)
#' grDevices::pdf(tempfile()); df2heatmap(mat, scale = "row", Colv = NA)
#' grDevices::dev.off()
#'
#' @export
df2heatmap <- function(df, clade = FALSE, trim = FALSE, ...) {
  invisible(stats::heatmap(t(df), col = rev(viridis::viridis(256)), ...))
}

#' Hierarchical clustering dendrogram
#'
#' Scales the matrix and draws a Ward.D2 dendrogram of either the taxa
#' (`use = "sp"`) or the samples (`use = "sample"`).
#'
#' @param df A numeric taxa-by-sample matrix, e.g. from [df_untidy()].
#' @param k_means Integer. Number of clusters to outline with rectangles
#'   (`< 2` draws no rectangles).
#' @param use `"sp"` to cluster taxa (rows) or `"sample"` to cluster columns.
#'
#' @return Invisibly `NULL`; a dendrogram is drawn as a side effect.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[13][124]_", trim_char = "_")
#' mat <- df_untidy(df, clade = "G", top = 20, scale = "log2")
#' grDevices::pdf(tempfile()); df2cluster(mat, k_means = 4, use = "sp")
#' grDevices::dev.off()
#'
#' @export
df2cluster <- function(df, k_means = 2, use = "sp") {
  rdf <- rownames(df)
  df <- apply(df, 2, scale)
  rownames(df) <- rdf

  if (use == "sp") {
    clust_res <- stats::hclust(stats::dist(df), method = "ward.D2")
  } else {
    clust_res <- stats::hclust(stats::dist(t(df)), method = "ward.D2")
  }

  graphics::plot(clust_res)
  if (k_means >= 2) stats::rect.hclust(clust_res, k = k_means, border = "red")
  invisible(NULL)
}

#' 2D cluster plot split by a legend variable
#'
#' Clusters taxa and plots each taxon by its mean (scaled) abundance in the two
#' sample groups defined by `legend_detect`.
#'
#' @param df A tidy `tibble` from [get_counts()].
#' @param legend_detect Character. Pattern matched against the encoded sample
#'   names to define group 1 (the complement is group 2). When a length-2 vector
#'   is supplied only the first element is used for the split.
#' @param clade Character or `FALSE`. Restrict to a single clade.
#' @param k_means Integer. Number of clusters.
#' @param counts Logical. Use raw counts (`N`) instead of `amount`.
#' @param top Integer or `FALSE`. Keep only the `top` most abundant taxa.
#' @param log2_scale Logical. Use `log2(x + 1)` instead of z-scaling.
#' @param ... Reserved for future use.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[1345][0-9]?_", legend = legend,
#'                  trim_char = "_")
#' df2clust2d(df[df$clade == "G", ], legend_detect = "pupa", top = 40,
#'            k_means = 5)
#'
#' @export
#' @importFrom rlang .data
df2clust2d <- function(df, legend_detect, clade = FALSE, k_means = 5,
                       counts = FALSE, top = FALSE, log2_scale = FALSE, ...) {

  amount_from <- if (counts) "N" else "amount"
  mat <- df_untidy(df, clade = clade, amount_from = amount_from,
                   top = top, keep_sample_name = FALSE)
  mat[is.na(mat)] <- 0
  rdf <- rownames(mat)

  if (log2_scale) {
    mat <- log2(mat + 1)
  } else {
    mat <- scale(mat)
  }
  rownames(mat) <- rdf

  clust_res <- stats::hclust(stats::dist(mat), method = "ward.D2")
  groups <- stats::cutree(clust_res, k = k_means)

  idx1 <- which(stringr::str_detect(colnames(mat), legend_detect[1]))
  idx2 <- which(stringr::str_detect(colnames(mat), legend_detect[1],
                                    negate = TRUE))

  res <- data.frame(
    name = rdf,
    x = rowMeans(mat[, idx1, drop = FALSE]),
    y = rowMeans(mat[, idx2, drop = FALSE]),
    cluster = groups)
  res$name[abs(res$x / res$y) < 1.1 & abs(res$x / res$y) > (1 / 1.1)] <- NA

  ggplot2::ggplot(res, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2,
                         color = "cadetblue") +
    ggplot2::geom_point(ggplot2::aes(color = .data$cluster)) +
    ggrepel::geom_label_repel(ggplot2::aes(label = .data$name)) +
    ggplot2::scale_color_continuous("Cluster", type = "viridis") +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal()
}

#' Correlation plot between taxa
#'
#' @param df A numeric taxa-by-sample matrix, e.g. from [df_untidy()].
#' @param k_means Integer or `FALSE`. Number of clusters to outline with
#'   rectangles.
#' @param ... Passed to [corrplot::corrplot].
#'
#' @return Invisibly, the correlation matrix (a plot is drawn as a side effect).
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[13][124]_", trim_char = "_")
#' mat <- df_untidy(df, clade = "G", top = 12, scale = "scale")
#' grDevices::pdf(tempfile()); df2corrplot(mat, k_means = 3)
#' grDevices::dev.off()
#'
#' @export
df2corrplot <- function(df, k_means = FALSE, ...) {
  cmat <- stats::cor(t(df))
  cmat[is.na(cmat)] <- 0

  corrplot::corrplot(cmat, is.corr = TRUE, hclust.method = "complete",
                     tl.col = "black", order = "hclust",
                     addrect = if (isFALSE(k_means)) NULL else k_means,
                     font = 3, ...)
  invisible(cmat)
}

#' Circular chord / connection plot of taxa correlations
#'
#' Builds a taxa-by-taxa correlation graph and draws it as a circular
#' \pkg{ggraph} layout, colouring edges either by correlation or by cluster.
#'
#' @param df A numeric taxa-by-sample matrix, e.g. from [df_untidy()].
#' @param clade Ignored; kept for backward compatibility.
#' @param k_means Integer. Number of clusters.
#' @param amount_from Ignored; kept for backward compatibility.
#' @param coenf_level Numeric or `FALSE`. Threshold below/above which
#'   connections are hidden.
#' @param coenf One of `"both"`, `"upper"`, `"lower"`; how `coenf_level` is
#'   applied.
#' @param line_as_clusters Logical. Colour edges by cluster instead of by
#'   correlation value.
#' @param ... Reserved for future use.
#'
#' @return A [ggplot2::ggplot] object (a \pkg{ggraph} plot).
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[13][124]_", trim_char = "_")
#' mat <- df_untidy(df, clade = "G", top = 12, scale = "scale")
#' df2chord(mat, k_means = 3, coenf_level = 0.5)
#'
#' @export
#' @importFrom rlang .data
df2chord <- function(df, clade = FALSE, k_means = 5, amount_from = "amount",
                     coenf_level = FALSE, coenf = "both",
                     line_as_clusters = FALSE, ...) {

  df <- stats::cor(t(df))
  df[is.na(df)] <- 0

  df_order <- corrplot::corrMatOrder(df, order = "hclust")
  corhcl <- stats::hclust(stats::dist(df))
  groups <- stats::cutree(corhcl, k = k_means)

  df <- df[df_order, df_order]
  groups <- groups[df_order]

  rdf <- colnames(df)
  ldfr <- length(rdf)

  h_remove <- c()
  for (i in seq_len(ldfr)) {
    h_remove <- c(h_remove, (ldfr * (i - 1) + 1):(ldfr * (i - 1) + (i - 1)))
  }
  h_remove <- h_remove[-(1:2)]

  vertices <- tidyr::pivot_longer(as.data.frame(df), cols = seq_len(ldfr))

  angle <- 90 - 360 * 0:(ldfr - 1) / ldfr
  hjust <- ifelse(angle < -90, 1, 0)
  angle <- ifelse(angle < -90, angle + 180, angle)

  vals <- unlist(vertices[-h_remove, 2])

  gg <- igraph::graph_from_adjacency_matrix(df, mode = "undirected",
                                            weighted = TRUE)

  if (line_as_clusters) {
    df_clust <- data.frame(name = rdf, group = groups)
    df_clust <- dplyr::left_join(vertices, df_clust, by = "name")[-h_remove, 3]

    if (!isFALSE(coenf_level)) {
      if (coenf == "upper") {
        df_clust[vals < coenf_level, 1] <- NA
      } else if (coenf == "lower") {
        df_clust[vals > coenf_level, 1] <- NA
      } else {
        df_clust[abs(vals) < coenf_level, 1] <- NA
      }
    }

    ggraph::ggraph(gg, layout = "linear", circular = TRUE) +
      ggraph::geom_edge_arc(
        ggplot2::aes(alpha = (vals)^4,
                     color = as.character(df_clust$group)),
        show.legend = FALSE) +
      ggraph::geom_node_point(
        ggplot2::aes(x = .data$x * 1.05, y = .data$y * 1.05,
                     color = as.character(groups)), show.legend = FALSE) +
      ggraph::geom_node_text(
        ggplot2::aes(x = .data$x * 1.1, y = .data$y * 1.1, label = .data$name,
                     angle = angle, hjust = hjust), fontface = "italic") +
      ggplot2::scale_color_manual(values = viridis::viridis(k_means)) +
      ggraph::scale_edge_color_manual(values = viridis::viridis(k_means),
                                      na.value = "transparent",
                                      guide = "none") +
      ggraph::scale_edge_alpha_continuous(range = c(0, 0.5), na.value = 0,
                                          guide = "none") +
      ggplot2::coord_fixed() +
      ggplot2::theme_void() +
      ggplot2::expand_limits(x = c(-3, 3), y = c(-3, 3))
  } else {
    if (!isFALSE(coenf_level)) {
      if (coenf == "upper") {
        vals[vals < coenf_level] <- NA
      } else if (coenf == "lower") {
        vals[vals > coenf_level] <- NA
      } else {
        vals[abs(vals) < coenf_level] <- NA
      }
    }

    ggraph::ggraph(gg, layout = "linear", circular = TRUE) +
      ggraph::geom_edge_arc(
        ggplot2::aes(alpha = (vals)^2, color = vals), show.legend = FALSE) +
      ggraph::geom_node_point(
        ggplot2::aes(x = .data$x * 1.05, y = .data$y * 1.05,
                     color = as.character(groups)), show.legend = FALSE) +
      ggraph::geom_node_text(
        ggplot2::aes(x = .data$x * 1.1, y = .data$y * 1.1, label = .data$name,
                     angle = angle, hjust = hjust), fontface = "italic") +
      ggplot2::scale_color_manual(values = viridis::viridis(k_means)) +
      ggraph::scale_edge_color_gradient2(low = "red", mid = "white",
                                         high = "blue",
                                         na.value = "transparent",
                                         guide = "none") +
      ggraph::scale_edge_alpha_continuous(range = c(0, 0.5), guide = "none") +
      ggplot2::coord_fixed() +
      ggplot2::theme_void() +
      ggplot2::expand_limits(x = c(-3, 3), y = c(-3, 3))
  }
}

#' t-SNE ordination of taxa
#'
#' Runs t-SNE (via \pkg{tsne}) on a taxa-by-sample matrix and plots the taxa in
#' two dimensions, coloured by hierarchical cluster.
#'
#' @param df A numeric taxa-by-sample matrix, e.g. from [df_untidy()].
#' @param color Either `"clust"` (default; colour by hierarchical cluster) or a
#'   vector of length `nrow(df)` giving a grouping per taxon.
#' @param k_means Integer. Number of clusters when `color = "clust"`.
#' @param text_top Integer or `FALSE`. Label only the `text_top` most abundant
#'   taxa (`-1` disables labels entirely).
#' @param perplexity Numeric. t-SNE perplexity (kept small speeds things up).
#' @param max_iter Integer. Maximum t-SNE iterations.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[13][124]_", trim_char = "_")
#' mat <- df_untidy(df, clade = "G", top = 30, scale = "scale")
#' set.seed(1)
#' df2tsne(mat, k_means = 4, text_top = 5, perplexity = 5, max_iter = 100)
#'
#' @export
#' @importFrom rlang .data
df2tsne <- function(df, color = "clust", k_means = 10, text_top = FALSE,
                    perplexity = 30, max_iter = 1000) {

  if (length(color) == nrow(df)) {
    groups <- data.frame(taxa = rownames(df), clust = color)
  } else {
    clust_res <- stats::hclust(stats::dist(df), method = "ward.D2")
    groups <- data.frame(taxa = clust_res$labels,
                         clust = stats::cutree(clust_res, k = k_means))
  }

  tsne_out <- tsne::tsne(df, perplexity = perplexity, max_iter = max_iter)
  tsne_plot <- data.frame(taxa = rownames(df),
                          x = tsne_out[, 1],
                          y = tsne_out[, 2]) %>%
    dplyr::left_join(groups, by = "taxa")

  if (identical(text_top, -1)) {
    tsne_plot$taxa <- NA
  } else if (!isFALSE(text_top)) {
    df <- df[order(apply(df, 1, mean), decreasing = TRUE), ]
    top <- rownames(df)[seq_len(min(text_top, nrow(df)))]
    tsne_plot$taxa[!(tsne_plot$taxa %in% top)] <- NA
  }

  message("tSNE calculated")

  tsne_plot$clust <- factor(as.character(tsne_plot$clust))
  lvir <- length(levels(tsne_plot$clust))

  ggplot2::ggplot(tsne_plot, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$clust), show.legend = TRUE,
                        alpha = 0.5) +
    ggrepel::geom_label_repel(ggplot2::aes(label = .data$taxa), size = 5,
                              max.overlaps = 100) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::scale_color_discrete("", type = viridis::viridis(lvir)) +
    ggplot2::theme_minimal()
}

#' Volcano plot of abundance change between two sample groups
#'
#' Runs a per-taxon t-test between the two groups defined by `legend_detect` and
#' plots the log abundance change against the t-test p-value.
#'
#' @param df A tidy `tibble` from [get_counts()] with legend columns.
#' @param legend_detect Length-2 character vector of patterns identifying the
#'   two groups.
#' @param treshhold_logAC Numeric. Absolute log abundance-change threshold for
#'   labelling.
#' @param treshhold_p Numeric. P-value threshold for labelling.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[1345][0-9]?_", legend = legend,
#'                  trim_char = "_")
#' df2volcano(df[df$clade == "G", ], legend_detect = c("pupa", "larvae"))
#'
#' @export
#' @importFrom rlang .data
df2volcano <- function(df, legend_detect, treshhold_logAC = 0.5,
                       treshhold_p = 0.05) {

  df_un <- df %>% df_untidy(keep_sample_name = FALSE)
  df1 <- df_un[, stringr::str_detect(colnames(df_un), legend_detect[1]),
               drop = FALSE]
  df2 <- df_un[, stringr::str_detect(colnames(df_un), legend_detect[2]),
               drop = FALSE]

  t_df <- vapply(seq_len(nrow(df_un)), function(i) {
    if ((sum(df1[i, ]) != 0) && (sum(df2[i, ]) != 0)) {
      tryCatch(stats::t.test(df1[i, ], df2[i, ], paired = FALSE)$p.value,
               error = function(e) NA_real_)
    } else {
      NA_real_
    }
  }, numeric(1))

  t_df <- data.frame(taxa = row.names(df_un),
                     p = -log10(t_df),
                     sd = apply(df_un, 1, stats::sd))

  df_str <- apply(df[, -c(1:6)], 1,
                  function(z) stringr::str_c(z, collapse = "_"))
  df1mean <- dplyr::summarise(
    df[stringr::str_detect(df_str, legend_detect[1]), ],
    x = mean(amount), .by = "taxa")
  df2mean <- dplyr::summarise(
    df[stringr::str_detect(df_str, legend_detect[2]), ],
    y = mean(amount), .by = "taxa")

  res <- as.data.frame(
    dplyr::left_join(dplyr::full_join(df1mean, df2mean, by = "taxa"),
                     t_df, by = "taxa"))
  res[is.na(res)] <- 0
  res$logAC <- log10(res$y / res$x)

  res$taxa[(abs(res$logAC) < treshhold_logAC) |
             (res$p < -log10(treshhold_p))] <- NA

  ggplot2::ggplot(res, ggplot2::aes(.data$logAC, .data$p)) +
    ggplot2::geom_point(ggplot2::aes(color = abs(.data$logAC * .data$p),
                                     size = .data$x + .data$y),
                        show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = -log10(treshhold_p), linetype = 3,
                        alpha = 0.5, color = "red") +
    ggplot2::geom_vline(xintercept = treshhold_logAC, linetype = 3,
                        alpha = 0.5, color = "red") +
    ggplot2::geom_vline(xintercept = -treshhold_logAC, linetype = 3,
                        alpha = 0.5, color = "red") +
    ggrepel::geom_label_repel(ggplot2::aes(label = .data$taxa),
                              max.overlaps = 30) +
    ggplot2::scale_color_gradient("logAC", high = "blue", low = "gray",
                                  na.value = "blue") +
    ggplot2::xlab("log10 amount change") +
    ggplot2::ylab("p-value by t.test") +
    ggplot2::theme_minimal()
}

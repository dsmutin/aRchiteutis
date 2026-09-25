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

  factoextra::fviz_pca_ind(res_pca, col.ind = color_list, repel = TRUE, ...) +
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

  factoextra::fviz_pca_var(res_pca, repel = TRUE, ...) +
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
  invisible(stats::heatmap(t(df), col = rev(viridis::viridis(256)),
                           margins = c(12, 8), ...))
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

  nlab <- length(clust_res$labels)
  cex <- max(0.45, min(0.75, 18 / nlab))
  dend <- stats::as.dendrogram(clust_res)
  if (k_means >= 2 && k_means < nlab) {
    groups <- stats::cutree(clust_res, k = k_means)
    cols <- viridis::viridis(k_means)
    dend <- stats::dendrapply(dend, function(node) {
        if (stats::is.leaf(node)) {
        lab <- attr(node, "label")
        attr(node, "nodePar") <- list(
          pch = NA, cex = 0, lab.cex = cex,
          lab.col = cols[groups[[lab]]])
      }
      node
    })
  }
  op <- graphics::par(mar = c(1, 2, 1, 10), no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)
  graphics::plot(dend, horiz = TRUE, axes = FALSE, main = "",
                 xlab = "", ylab = "")
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
#' @param log2_scale Logical. Use a log2 axis instead of log10. Axes are always
#'   logarithmic scales of mean abundance, not a linear plot of pre-transformed
#'   values.
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

  idx1 <- which(stringr::str_detect(colnames(mat), legend_detect[1]))
  idx2 <- which(stringr::str_detect(colnames(mat), legend_detect[1],
                                    negate = TRUE))
  if (!length(idx1) || !length(idx2)) {
    stop("`legend_detect` did not split the samples into two non-empty groups.")
  }

  # Cluster in log space, but plot the means on a real log axis.
  pos <- mat[mat > 0]
  floor_val <- if (length(pos)) min(pos) / 2 else 1e-6
  clust_mat <- log10(mat + floor_val)
  rownames(clust_mat) <- rdf
  clust_res <- stats::hclust(stats::dist(clust_mat), method = "ward.D2")
  groups <- stats::cutree(clust_res, k = k_means)

  res <- data.frame(
    name = rdf,
    x = rowMeans(mat[, idx1, drop = FALSE]),
    y = rowMeans(mat[, idx2, drop = FALSE]),
    cluster = groups)
  res$x[res$x <= 0] <- floor_val
  res$y[res$y <= 0] <- floor_val
  res$name[abs(res$x / res$y) < 1.1 & abs(res$x / res$y) > (1 / 1.1)] <- NA

  group_title <- function(cols, fallback) {
    tok <- vapply(strsplit(cols, "_"), function(z) z[[1]], character(1))
    tok <- unique(tok[nzchar(tok)])
    if (!length(tok)) fallback else paste(tok, collapse = ", ")
  }
  x_title <- if (length(legend_detect) >= 1 && nzchar(legend_detect[1])) {
    legend_detect[1]
  } else {
    group_title(colnames(mat)[idx1], "group 1")
  }
  y_title <- if (length(legend_detect) >= 2 && nzchar(legend_detect[2])) {
    legend_detect[2]
  } else {
    group_title(colnames(mat)[idx2], "other")
  }

  log_scale <- if (isTRUE(log2_scale)) {
    list(ggplot2::scale_x_continuous(trans = "log2"),
         ggplot2::scale_y_continuous(trans = "log2"))
  } else {
    list(ggplot2::scale_x_log10(), ggplot2::scale_y_log10())
  }

  ggplot2::ggplot(res, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2,
                         color = "cadetblue") +
    ggplot2::geom_point(ggplot2::aes(color = .data$cluster), size = 2) +
    ggrepel::geom_label_repel(
      ggplot2::aes(label = .data$name),
      size = 2.6, max.overlaps = 40, box.padding = 0.35,
      point.padding = 0.3, min.segment.length = 0, seed = 1,
      na.rm = TRUE) +
    ggplot2::scale_color_continuous("Cluster", type = "viridis") +
    log_scale +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::xlab(x_title) +
    ggplot2::ylab(y_title) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.margin = ggplot2::margin(16, 16, 16, 16))
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
                     tl.col = "black", tl.cex = 0.7, order = "hclust",
                     addrect = if (isFALSE(k_means)) NULL else k_means,
                     font = 3, mar = c(1, 1, 1, 1), ...)
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
  lab_size <- max(6.4, min(8.4, 240 / ldfr))
  lab_r <- if (ldfr > 20) 1.28 else 1.16
  lab_pad <- if (ldfr > 20) 3.05 else 2.15

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
                     color = as.character(df_clust$group))) +
      ggraph::geom_node_point(
        ggplot2::aes(x = .data$x * 1.05, y = .data$y * 1.05,
                     color = as.character(groups)), show.legend = FALSE) +
      ggraph::geom_node_text(
        ggplot2::aes(x = .data$x * lab_r, y = .data$y * lab_r, label = .data$name,
                     angle = angle, hjust = hjust),
        size = lab_size, fontface = "italic",
        check_overlap = ldfr > 36) +
      ggplot2::scale_color_manual(values = viridis::viridis(k_means), guide = "none") +
      ggraph::scale_edge_color_manual(
        values = viridis::viridis(k_means),
        na.value = "transparent",
        guide = "none") +
      ggraph::scale_edge_alpha_continuous(range = c(0, 0.5), na.value = 0,
                                          guide = "none") +
      ggnewscale::new_scale_colour() +
      ggplot2::geom_point(
        data = data.frame(cluster = factor(seq_len(k_means))),
        ggplot2::aes(x = 0, y = 0, colour = .data$cluster),
        inherit.aes = FALSE, alpha = 0, size = 0) +
      ggplot2::scale_colour_manual(
        values = stats::setNames(viridis::viridis(k_means), seq_len(k_means)),
        name = "cluster",
        guide = ggplot2::guide_legend(
          nrow = 1, title.position = "top", title.hjust = 0.5,
          override.aes = list(alpha = 1, size = 5))) +
      ggplot2::coord_fixed(clip = "off") +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 11),
        legend.margin = ggplot2::margin(8, 0, 4, 0),
        plot.margin = ggplot2::margin(16, 20, 22, 20)) +
      ggplot2::expand_limits(x = c(-lab_pad, lab_pad), y = c(-lab_pad, lab_pad))
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
        ggplot2::aes(alpha = (vals)^2, color = vals)) +
      ggraph::geom_node_point(
        ggplot2::aes(x = .data$x * 1.05, y = .data$y * 1.05,
                     color = as.character(groups)), show.legend = FALSE) +
      ggraph::geom_node_text(
        ggplot2::aes(x = .data$x * lab_r, y = .data$y * lab_r, label = .data$name,
                     angle = angle, hjust = hjust),
        size = lab_size, fontface = "italic",
        check_overlap = ldfr > 36) +
      ggplot2::scale_color_manual(values = viridis::viridis(k_means), guide = "none") +
      ggraph::scale_edge_color_gradient2(
        low = "red", mid = "white", high = "blue",
        na.value = "transparent",
        guide = "none") +
      ggraph::scale_edge_alpha_continuous(range = c(0, 0.5), guide = "none") +
      ggnewscale::new_scale_colour() +
      ggplot2::geom_point(
        data = data.frame(corr = range(vals, na.rm = TRUE)),
        ggplot2::aes(x = 0, y = 0, colour = .data$corr),
        inherit.aes = FALSE, alpha = 0, size = 0) +
      ggplot2::scale_colour_gradient2(
        low = "red", mid = "white", high = "blue", midpoint = 0,
        name = "correlation",
        guide = ggplot2::guide_colourbar(
          title.position = "top", title.hjust = 0.5,
          barwidth = ggplot2::unit(14, "cm"),
          barheight = ggplot2::unit(1.5, "cm"))) +
      ggplot2::coord_fixed(clip = "off") +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = ggplot2::element_text(size = 14),
        legend.text = ggplot2::element_text(size = 12),
        legend.margin = ggplot2::margin(14, 0, 8, 0),
        plot.margin = ggplot2::margin(18, 24, 28, 24)) +
      ggplot2::expand_limits(x = c(-lab_pad, lab_pad), y = c(-lab_pad, lab_pad))
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
    ggrepel::geom_label_repel(ggplot2::aes(label = .data$taxa), size = 2.8,
                              max.overlaps = 30, box.padding = 0.4,
                              point.padding = 0.25, min.segment.length = 0,
                              seed = 1, na.rm = TRUE) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::scale_color_discrete("", type = viridis::viridis(lvir)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.margin = ggplot2::margin(14, 14, 14, 14))
}

#' Volcano plot of ANCOM-BC2 log fold change
#'
#' Fits [ANCOMBC::ancombc2] on raw counts (`N`) for the two groups matched by
#' `legend_detect` (sample names or legend fields). Counts are passed as a
#' \pkg{phyloseq} object, which ANCOM-BC2 converts to a tree experiment. The
#' x axis is the bias-corrected log2 fold change of the second group against
#' the first, and the y axis is `-log10` of the adjusted q-value. Taxa with
#' no counts in one of the two groups are omitted: ANCOM-BC2 cannot estimate
#' a sampling variance for them. The function stops when \pkg{ANCOMBC} or
#' \pkg{phyloseq} is not installed.
#'
#' @param df A tidy `tibble` from [get_counts()] with legend columns.
#' @param legend_detect Length-2 character vector of patterns identifying the
#'   two groups. The first pattern is the ANCOM-BC reference level.
#' @param treshhold_logAC Numeric. Absolute log2 fold-change threshold for
#'   labelling.
#' @param treshhold_p Numeric. Adjusted q-value threshold for labelling.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m(11|12|13|18|4|39)_",
#'                  legend = legend, trim_char = "_")
#' if (requireNamespace("ANCOMBC", quietly = TRUE) &&
#'     requireNamespace("phyloseq", quietly = TRUE)) {
#'   g <- df[df$clade == "G", ]
#'   keep <- names(sort(tapply(g$N, g$taxa, sum), decreasing = TRUE))[seq_len(30)]
#'   df2volcano(g[g$taxa %in% keep, ], legend_detect = c("larvae", "pupa"))
#' }
#'
#' @export
#' @importFrom rlang .data
df2volcano <- function(df, legend_detect, treshhold_logAC = 0.5,
                       treshhold_p = 0.05) {
  archi_optional("ANCOMBC", "df2volcano")
  archi_optional("phyloseq", "df2volcano")
  if (length(legend_detect) != 2L) {
    stop("legend_detect must name two groups", call. = FALSE)
  }
  res <- archi_ancombc_volcano(df, legend_detect)
  res$neglog10q <- -log10(pmax(res$q, .Machine$double.xmin))
  res$diffexpressed <- "NS"
  ok <- !is.na(res$log2_lfc) & !is.na(res$q)
  res$diffexpressed[ok & res$q < treshhold_p & res$log2_lfc >= treshhold_logAC] <- "Up"
  res$diffexpressed[ok & res$q < treshhold_p & res$log2_lfc <= -treshhold_logAC] <- "Down"
  res$label <- res$taxa
  res$label[res$diffexpressed == "NS"] <- NA_character_

  ggplot2::ggplot(res, ggplot2::aes(.data$log2_lfc, .data$neglog10q,
                                    color = .data$diffexpressed)) +
    ggplot2::geom_point(alpha = 0.75, size = 1.6) +
    ggplot2::geom_hline(yintercept = -log10(treshhold_p), linetype = 2,
                        linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = c(-treshhold_logAC, treshhold_logAC),
                        linetype = 2, linewidth = 0.3) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = .data$label), size = 2.4, max.overlaps = 15,
      show.legend = FALSE, na.rm = TRUE, seed = 1
    ) +
    ggplot2::scale_color_manual(
      values = c(Up = "#D81B60", Down = "#1B9E77", NS = "grey70"),
      name = NULL
    ) +
    ggplot2::labs(
      x = expression(log[2]~fold~change),
      y = expression(-log[10]~adjusted~italic(q))
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(legend.position = "top")
}

#' ANCOM-BC2 log2 fold change for two legend groups
#' @keywords internal
archi_ancombc_volcano <- function(df, legend_detect) {
  grouped <- archi_legend_groups(df, legend_detect)
  if (length(grouped$samples1) < 2L || length(grouped$samples2) < 2L) {
    stop(
      "ANCOM-BC2 needs at least two samples in each group (found ",
      length(grouped$samples1), " and ", length(grouped$samples2), ")",
      call. = FALSE
    )
  }
  keep <- c(grouped$samples1, grouped$samples2)
  sub <- df[as.character(df$sample) %in% keep, , drop = FALSE]
  mat <- df_untidy(sub, amount_from = "N", drop_unclassified = TRUE)
  mat <- mat[, intersect(keep, colnames(mat)), drop = FALSE]
  mat <- round(mat)
  mat[mat < 0 | !is.finite(mat)] <- 0
  storage.mode(mat) <- "integer"
  in1 <- colnames(mat) %in% grouped$samples1
  # A taxon missing from an entire group has a zero sampling variance, and
  # ANCOM-BC2 stops in the bias step. Keep taxa seen in both groups.
  seen <- rowSums(mat[, in1, drop = FALSE] > 0) > 0 &
    rowSums(mat[, !in1, drop = FALSE] > 0) > 0
  mat <- mat[seen & apply(mat, 1, stats::var) > 0, , drop = FALSE]
  if (nrow(mat) < 2L) {
    stop("ANCOM-BC2 needs taxa present in both groups", call. = FALSE)
  }
  group <- factor(
    ifelse(colnames(mat) %in% grouped$samples1, legend_detect[[1]], legend_detect[[2]]),
    levels = legend_detect
  )
  sam <- phyloseq::sample_data(data.frame(
    group = group, row.names = colnames(mat), stringsAsFactors = FALSE
  ))
  fit <- NULL
  for (attempt in seq_len(8L)) {
    ps <- phyloseq::phyloseq(
      phyloseq::otu_table(mat, taxa_are_rows = TRUE), sam
    )
    fit <- tryCatch(
      ANCOMBC::ancombc2(
        data = ps,
        fix_formula = "group",
        p_adj_method = "fdr",
        prv_cut = 0.1,
        lib_cut = 0,
        group = "group",
        struc_zero = FALSE,
        neg_lb = FALSE,
        pseudo = 0,
        pseudo_sens = FALSE,
        global = FALSE,
        pairwise = FALSE,
        verbose = FALSE,
        n_cl = 1L
      ),
      error = function(e) e
    )
    if (!inherits(fit, "error")) break
    msg <- conditionMessage(fit)
    if (!grepl("Zero variances have been detected", msg, fixed = TRUE)) {
      stop(msg, call. = FALSE)
    }
    inner <- sub(".*following taxa:\\s*", "", msg)
    inner <- sub("\\s*Please remove these taxa.*", "", inner)
    bad <- trimws(strsplit(inner, ",\\s*")[[1]])
    bad <- bad[nzchar(bad)]
    mat <- mat[!rownames(mat) %in% bad, , drop = FALSE]
    if (nrow(mat) < 2L || !length(bad)) stop(msg, call. = FALSE)
  }
  if (inherits(fit, "error")) stop(conditionMessage(fit), call. = FALSE)
  res <- fit$res
  if (is.null(res) || !nrow(res)) stop("ANCOM-BC2 returned no taxa", call. = FALSE)
  lfc_col <- grep("^lfc_", names(res), value = TRUE)
  lfc_col <- setdiff(lfc_col, "lfc_(Intercept)")
  if (!length(lfc_col)) stop("ANCOM-BC2 returned no log-fold-change column", call. = FALSE)
  term <- sub("^lfc_", "", lfc_col[[1]])
  q_col <- paste0("q_", term)
  if (!q_col %in% names(res)) q_col <- paste0("p_", term)
  taxon <- if ("taxon" %in% names(res)) res$taxon else res[[1]]
  data.frame(
    taxa = as.character(taxon),
    log2_lfc = as.numeric(res[[lfc_col[[1]]]]),
    q = as.numeric(res[[q_col]]),
    stringsAsFactors = FALSE
  )
}

#' Samples matching two legend patterns
#' @keywords internal
archi_legend_groups <- function(df, legend_detect) {
  meta_cols <- setdiff(names(df), c(
    "taxa", "clade", "sample", "N", "amount", "amount_cl"
  ))
  samples <- unique(as.character(df$sample))
  if (length(meta_cols)) {
    meta <- unique(df[, c("sample", meta_cols), drop = FALSE])
    key <- apply(meta[, meta_cols, drop = FALSE], 1, function(z) paste(z, collapse = "_"))
    names(key) <- as.character(meta$sample)
  } else {
    key <- stats::setNames(samples, samples)
  }
  key[is.na(key)] <- ""
  samples1 <- names(key)[stringr::str_detect(key, legend_detect[[1]]) |
                            stringr::str_detect(names(key), legend_detect[[1]])]
  samples2 <- names(key)[stringr::str_detect(key, legend_detect[[2]]) |
                            stringr::str_detect(names(key), legend_detect[[2]])]
  samples2 <- setdiff(samples2, samples1)
  list(samples1 = unique(samples1), samples2 = unique(samples2))
}

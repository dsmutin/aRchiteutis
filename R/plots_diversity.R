#' Summary of many alpha-diversity metrics per sample
#'
#' Computes a broad panel of alpha-diversity indices (from the \pkg{abdiv}
#' package) for every sample and displays them as faceted violin + jitter plots.
#'
#' @param df A tidy `tibble` from [get_counts()]. Retain species-level rows for
#'   meaningful diversity estimates.
#' @param split_by Integer column index (or `FALSE`) used to split the samples
#'   into groups on the x-axis.
#' @param add_legend Integer column index/indices (or `FALSE`) whose values are
#'   combined into the colour legend.
#' @param style One of `"violin"` (violin with a box of the same fill inside),
#'   `"box"`, or `"raincloud"` (half-violin and half-box from \pkg{ggviolinbox}).
#' @param ... Passed to the geoms.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m1[124]_", legend = legend,
#'                  trim_char = "_")
#' df2alpha_summary(df[df$clade == "S", ])
#' if (requireNamespace("ggviolinbox", quietly = TRUE)) {
#'   df2alpha_summary(df[df$clade == "S", ], split_by = 7, style = "raincloud")
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom abdiv dominance simpson simpson_e invsimpson shannon brillouin_d
#'   heip_e pielou_e strong mcintosh_d berger_parker_d richness mcintosh_e
#'   menhinick margalef kempton_taylor_q bray_curtis
df2alpha_summary <- function(df, split_by = FALSE, add_legend = FALSE,
                           style = c("violin", "box", "raincloud"), ...) {
  style <- match.arg(style)
  gg <- df %>%
    dplyr::group_by(sample) %>%
    dplyr::summarise(dplyr::across(N, list(
      Dominance = dominance,
      Simpson = simpson,
      "Simpson's evenness" = simpson_e,
      "Inverted simpson" = invsimpson,

      Shannon = shannon,
      "Brillouin's dominance" = brillouin_d,
      "Heip's evenness" = heip_e,
      "Pielou's evenness" = pielou_e,

      "Strong's dominance" = strong,
      "Mcintosh's dominance" = mcintosh_d,
      "Berger&Parker's dominance" = berger_parker_d,
      Richness = richness,

      "Mcintosh's evenness" = mcintosh_e,
      "Menhinick's richnes" = menhinick,
      "Margalef's richness" = margalef,
      "Kempton&Taylor Q" = kempton_taylor_q
    )))

  gg <- tidyr::pivot_longer(gg, cols = -1)
  gg$name <- forcats::fct_inorder(factor(stringr::str_remove_all(gg$name, "N_")))

  df2diversity_plot(df, gg, split_by, add_legend, style = style, ...)
}

#' Alpha diversity for a chosen set of metrics
#'
#' A lighter version of [df2alpha_summary()] using a user-supplied list of
#' diversity functions.
#'
#' @inheritParams df2alpha_summary
#' @param alpha_function_list Named list of diversity functions (defaults to
#'   Shannon and Simpson from \pkg{abdiv}).
#' @param style One of `"box"` (default), `"violin"`, or `"raincloud"`.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m1[124]_", trim_char = "_")
#' df2alpha(df[df$clade == "S", ])
#'
#' @export
#' @importFrom rlang .data
df2alpha <- function(df, split_by = FALSE, add_legend = FALSE,
                     alpha_function_list = list(Shannon = abdiv::shannon,
                                                Simpson = abdiv::simpson),
                     style = c("box", "violin", "raincloud"),
                     ...) {
  style <- match.arg(style)
  gg <- df %>%
    dplyr::group_by(sample) %>%
    dplyr::summarise(dplyr::across(N, alpha_function_list))

  gg <- tidyr::pivot_longer(gg, cols = -1)
  gg$name <- forcats::fct_inorder(factor(stringr::str_remove_all(gg$name, "N_")))

  df2diversity_plot(df, gg, split_by, add_legend, style = style, ...)
}

# Half-violin + half-box from ggviolinbox (harness raincloud style).
archi_rain_layers <- function(fill_mapping = NULL, constant_fill = NULL,
                              orientation = NA) {
  if (!requireNamespace("ggviolinbox", quietly = TRUE)) {
    stop(
      "style = \"raincloud\" needs ggviolinbox. Install it with ",
      "remotes::install_github(\"dsmutin/ggviolinbox\").",
      call. = FALSE
    )
  }
  extra <- list(panel = "right", nudge = 0.08, trim = TRUE, width = 0.5,
                alpha = 0.45, colour = "grey35", show.legend = FALSE,
                orientation = orientation)
  box_extra <- list(panel = "left", nudge = -0.08, width = 0.22,
                    outliers = FALSE, alpha = 0.95, colour = "grey20",
                    show.legend = FALSE, orientation = orientation)
  if (!is.null(constant_fill)) {
    extra$fill <- constant_fill
    box_extra$fill <- constant_fill
  }
  violin <- do.call(ggviolinbox::geom_halfviolin, c(list(mapping = fill_mapping), extra))
  box <- do.call(ggviolinbox::geom_halfboxplot, c(list(mapping = fill_mapping), box_extra))
  list(violin, box)
}

# Shared plotting backend for df2alpha() / df2alpha_summary().
df2diversity_plot <- function(df, gg, split_by, add_legend,
                              style = c("violin", "box", "raincloud"), ...) {
  style <- match.arg(style)
  if (!isFALSE(add_legend)) {
    leg <- unique(df[, c(3, add_legend)])
    leg[, 2] <- apply(leg[, -1, drop = FALSE], 1,
                      function(z) stringr::str_c(z, collapse = ", "))
    gg <- dplyr::left_join(leg[, 1:2], gg, by = "sample")
    colnames(gg)[2] <- "sample2"
  } else {
    leg <- unique(df[, 3])
    gg$sample2 <- gg$sample
  }

  if (!isFALSE(split_by)) {
    leg2 <- unique(df[, c(3, split_by)])
    colnames(leg2)[2] <- "split"
    gg <- dplyr::left_join(leg2[, 1:2], gg, by = "sample")

    p <- ggplot2::ggplot(gg, ggplot2::aes(y = .data$value, x = .data$split))
    p <- p + if (identical(style, "raincloud")) {
      archi_rain_layers(ggplot2::aes(fill = .data$split))
    } else if (identical(style, "violin")) {
      list(
        ggplot2::geom_violin(trim = TRUE, scale = "width",
                             ggplot2::aes(fill = .data$split), alpha = 0.35,
                             colour = "grey35"),
        ggplot2::geom_boxplot(width = 0.14, outlier.shape = NA,
                              ggplot2::aes(fill = .data$split),
                              alpha = 0.95, colour = "grey20",
                              show.legend = FALSE)
      )
    } else {
      ggplot2::geom_boxplot(outlier.alpha = 0,
                            ggplot2::aes(fill = .data$split), alpha = 0.3)
    }
    same_legend <- identical(sort(unique(as.character(gg$sample2))),
                             sort(unique(as.character(gg$split))))
    p +
      ggplot2::geom_jitter(ggplot2::aes(color = .data$sample2),
                           width = 0.12, height = 0, size = 1.6,
                           show.legend = !same_legend) +
      ggplot2::facet_wrap(~name, drop = TRUE, ncol = 4, scales = "free_y",
                          strip.position = "top") +
      ggplot2::scale_color_discrete(
        NULL, type = viridis::viridis(length(unique(gg$sample2)))) +
      ggplot2::scale_fill_discrete(NULL) +
      ggplot2::ylab("") + ggplot2::xlab("") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.text = ggplot2::element_text(size = 8),
        legend.key.size = ggplot2::unit(0.35, "cm"),
        legend.spacing.x = ggplot2::unit(0.4, "cm"),
        strip.text = ggplot2::element_text(size = 8),
        axis.text.x = ggplot2::element_text(size = 8),
        plot.margin = ggplot2::margin(6, 8, 6, 6)) +
      ggplot2::guides(
        fill = "none",
        color = if (same_legend) "none" else
          ggplot2::guide_legend(nrow = 2, override.aes = list(size = 2)))
  } else {
    p <- ggplot2::ggplot(gg, ggplot2::aes(x = .data$value, y = .data$name))
    p <- p + if (identical(style, "raincloud")) {
      archi_rain_layers(constant_fill = "grey80", orientation = "y")
    } else if (identical(style, "violin")) {
      list(
        ggplot2::geom_violin(trim = TRUE, scale = "width",
                             fill = "grey80", colour = "grey35", alpha = 0.6),
        ggplot2::geom_boxplot(width = 0.14, outlier.shape = NA,
                              fill = "grey80", colour = "grey20",
                              show.legend = FALSE)
      )
    } else {
      ggplot2::geom_boxplot(outlier.alpha = 0)
    }
    p +
      ggplot2::geom_jitter(ggplot2::aes(color = .data$sample2),
                           width = 0.12, height = 0, size = 1.6) +
      ggplot2::facet_wrap(~name, drop = TRUE, ncol = 4, scales = "free",
                          strip.position = "top") +
      ggplot2::scale_color_discrete(
        "", type = viridis::viridis(length(unique(gg$sample))),
        labels = as.character(unlist(leg[, ncol(as.data.frame(leg))]))) +
      ggplot2::ylab("") + ggplot2::xlab("") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "right")
  }
}

# Internal helper: turn a long df into a scaled, per-sample beta-distance matrix
df_beta_matrix <- function(df, dist_function) {
  df %>%
    df_untidy(drop_unclassified = TRUE, scale = FALSE) %>%
    as.data.frame() %>%
    lapply(function(x) x / sum(x)) %>%
    as.data.frame() %>%
    t() %>%
    usedist::dist_make(dist_function) %>%
    as.matrix()
}

#' Beta-diversity heatmap between samples
#'
#' Computes a pairwise beta-diversity distance matrix between samples and draws
#' it as a ggplot tile heatmap, samples ordered by hierarchical clustering.
#'
#' @param df A tidy `tibble` from [get_counts()].
#' @param clade Character or `NULL`. Restrict to a single clade.
#' @param dist_function Distance function from \pkg{abdiv}
#'   (default [abdiv::bray_curtis]).
#' @param treshhold_up,treshhold_down Upper / lower mean-abundance thresholds
#'   used to filter taxa.
#' @param add_legend Integer column index/indices (or `FALSE`) drawn as a
#'   coloured point next to each sample.
#' @param add_labels Integer column index/indices (or `FALSE`) used as axis
#'   labels instead of the sample name.
#' @param print_df Logical. If `TRUE`, return the distance matrix instead of
#'   drawing.
#' @param method Optional distance name from [archi_beta_methods()], including
#'   `"aitchison"` (`robCompositions::aDist`) and the phyloseq set
#'   (`"bray"`, `"jaccard"`, `"unifrac"`, `"wunifrac"`, `"jsd"`, `"dpcoa"`).
#'   When `NULL`, `dist_function` is used (Bray-Curtis by default).
#' @param ... Unused; kept for backward compatibility.
#'
#' @return Invisibly, the ggplot object (a heatmap is drawn as a side effect);
#'   or the distance matrix when `print_df = TRUE`.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[13][124]_", trim_char = "_")
#' d <- df2beta(df[df$clade == "G", ], print_df = TRUE)
#' dim(d)
#'
#' @export
df2beta <- function(df, clade = "G", dist_function = abdiv::bray_curtis,
                    treshhold_up = 1, treshhold_down = 0,
                    add_legend = FALSE, add_labels = FALSE,
                    print_df = FALSE, method = NULL, ...) {

  pallete <- viridis::viridis(256)

  if (!is.null(clade)) df <- df[df$clade %in% clade, ]

  group_of <- NULL
  if (!isFALSE(add_legend)) {
    leg <- unique(df[, c(3, add_legend), drop = FALSE])
    group_of <- stats::setNames(
      apply(leg[, -1, drop = FALSE], 1, function(z) paste(z, collapse = ", ")),
      as.character(leg[[1]]))
  }

  if (!isFALSE(add_labels)) {
    leg2 <- unique(df[, c(3, add_labels), drop = FALSE])
    lab_of <- stats::setNames(
      apply(leg2[, -1, drop = FALSE], 1, function(z) paste(z, collapse = ", ")),
      as.character(leg2[[1]]))
  } else {
    lab_of <- NULL
  }

  df_taxa <- dplyr::summarise(df, m = mean(amount_cl), .by = "taxa")
  df_taxa <- df_taxa[df_taxa$m < treshhold_up & df_taxa$m > treshhold_down, ]
  df_taxa <- as.character(unlist(df_taxa[, 1]))

  df <- df[df$taxa %in% df_taxa, ]

  if (print_df) {
    if (!is.null(method)) return(archi_distance_matrix(df, method))
    return(df_beta_matrix(df, dist_function))
  }

  draw <- function(df, ...) {
    mat <- if (!is.null(method)) {
      archi_distance_matrix(df, method)
    } else {
      df_beta_matrix(df, dist_function)
    }
    if (is.null(method)) mat[1, 1] <- 1
    ord <- stats::hclust(stats::as.dist(mat), method = "ward.D2")$order
    mat <- mat[ord, ord, drop = FALSE]
    long <- as.data.frame(as.table(mat), stringsAsFactors = FALSE)
    names(long) <- c("row", "col", "dist")
    samples <- rownames(mat)
    row_lab <- if (is.null(lab_of)) samples else unname(lab_of[samples])
    if (anyDuplicated(row_lab)) row_lab <- samples
    x_levels <- row_lab
    if (!is.null(group_of)) x_levels <- c(" ", row_lab)
    long$row <- factor(row_lab[match(as.character(long$row), samples)],
                       levels = rev(row_lab))
    long$col <- factor(row_lab[match(as.character(long$col), samples)],
                       levels = x_levels)

    p <- ggplot2::ggplot(long, ggplot2::aes(.data$col, .data$row, fill = .data$dist)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradientn(colours = pallete, name = "distance") +
      ggplot2::coord_fixed() +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
        axis.text.y = ggplot2::element_text(size = 8),
        panel.grid = ggplot2::element_blank(),
        legend.key.height = ggplot2::unit(1.1, "cm"),
        plot.margin = ggplot2::margin(12, 12, 12, 12))

    if (!is.null(group_of)) {
      ann <- data.frame(
        x = factor(" ", levels = c(" ", row_lab)),
        sample = factor(row_lab, levels = levels(long$row)),
        grp = unname(group_of[samples]))
      pal <- stats::setNames(viridis::viridis(length(unique(ann$grp))), unique(ann$grp))
      p <- p +
        ggplot2::geom_point(
          data = ann,
          ggplot2::aes(x = .data$x, y = .data$sample, colour = .data$grp),
          inherit.aes = FALSE, size = 2.4) +
        ggplot2::scale_color_manual(values = pal, name = NULL) +
        ggplot2::scale_x_discrete(drop = FALSE)
    }

    print(p)
    invisible(p)
  }

  invisible(draw(df))
}

#' Bray-Curtis beta-diversity heatmap
#'
#' Thin convenience wrapper around [df2beta()] that fixes the distance function
#' to Bray-Curtis.
#'
#' @inheritParams df2beta
#' @param ... Passed to [df2beta()].
#'
#' @return See [df2beta()].
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[13][124]_", trim_char = "_")
#' d <- df2beta_bray(df[df$clade == "G", ], print_df = TRUE)
#' dim(d)
#'
#' @export
df2beta_bray <- function(df, ...) {
  df2beta(df, dist_function = abdiv::bray_curtis, ...)
}

#' PCoA ordination of samples from a beta-diversity matrix
#'
#' @inheritParams df2beta
#' @param add_ellipse Integer column index/indices (or `FALSE`) used to draw
#'   grouping ellipses.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[13][124]_", legend = legend,
#'                  trim_char = "_")
#' df2beta_pcoa(df[df$clade == "G", ], add_legend = 7)
#'
#' @export
#' @importFrom rlang .data
df2beta_pcoa <- function(df, dist_function = abdiv::bray_curtis,
                         treshhold_up = 1, treshhold_down = 0,
                         add_legend = FALSE, add_ellipse = FALSE,
                         method = NULL, ...) {

  df_legend <- unique(df[, -c(1:2, 4:6)])

  if (!isFALSE(add_legend)) {
    leg1 <- apply(df_legend[, add_legend - 5, drop = FALSE], 1,
                  function(z) stringr::str_c(z, collapse = "_"))
  } else {
    leg1 <- as.character(df_legend$sample)
  }

  if (!isFALSE(add_ellipse)) {
    leg2 <- apply(df_legend[, add_ellipse - 5, drop = FALSE], 1,
                  function(z) stringr::str_c(z, collapse = "_"))
  } else {
    leg2 <- NULL
  }

  pcoa_df <- if (!is.null(method)) {
    ape::pcoa(archi_distance_matrix(df, method))
  } else {
    df %>%
      df_untidy(drop_unclassified = TRUE, scale = FALSE) %>%
      as.data.frame() %>%
      t() %>%
      usedist::dist_make(dist_function) %>%
      as.matrix() %>%
      ape::pcoa()
  }

  vectors <- as.data.frame(pcoa_df$vectors)
  vectors$leg1 <- forcats::fct_inorder(factor(leg1))

  gg <- ggplot2::ggplot(vectors, ggplot2::aes(.data$Axis.1, .data$Axis.2)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$leg1), size = 2.4) +
    ggplot2::scale_color_discrete(
      NULL, type = viridis::viridis(length(unique(leg1)))) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.margin = ggplot2::margin(8, 8, 8, 8))

  if (!is.null(leg2)) {
    vectors$leg2 <- leg2
    gg <- gg +
      ggnewscale::new_scale_colour() +
      ggforce::geom_mark_ellipse(
        data = vectors,
        ggplot2::aes(color = .data$leg2, group = .data$leg2),
        expand = ggplot2::unit(2, "mm"),
        show.legend = FALSE)
  }

  gg
}

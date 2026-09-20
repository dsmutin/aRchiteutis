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
#' @param violinbox Controls the distribution geom. `FALSE` (default) keeps the
#'   classic \pkg{ggplot2} `geom_violin` panel. `TRUE` or `"combined"` draws a
#'   combined half-violin + half-boxplot via
#'   [ggviolinbox::geom_violinboxplot()]; `"halves"` uses a
#'   [ggviolinbox::geom_halfviolin()] + [ggviolinbox::geom_halfboxplot()] pair.
#'   Any non-`FALSE` value requires the \pkg{ggviolinbox} package.
#' @param box_side,violin_side Side (`"left"` / `"right"`) each geom is drawn on
#'   when `violinbox` is enabled. Ignored when `violinbox = FALSE`.
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
#'   df2alpha_summary(df[df$clade == "S", ], violinbox = TRUE)
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom abdiv dominance simpson simpson_e invsimpson shannon brillouin_d
#'   heip_e pielou_e strong mcintosh_d berger_parker_d richness mcintosh_e
#'   menhinick margalef kempton_taylor_q bray_curtis
df2alpha_summary <- function(df, split_by = FALSE, add_legend = FALSE,
                             violinbox = FALSE, box_side = "left",
                             violin_side = "right", ...) {
  df <- as_samovar_df(df)
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

  df2diversity_plot(df, gg, split_by, add_legend, violin = TRUE,
                    violinbox = violinbox, box_side = box_side,
                    violin_side = violin_side, ...)
}

#' Alpha diversity for a chosen set of metrics
#'
#' A lighter version of [df2alpha_summary()] using a user-supplied list of
#' diversity functions.
#'
#' @inheritParams df2alpha_summary
#' @param alpha_function_list Named list of diversity functions (defaults to
#'   Shannon and Simpson from \pkg{abdiv}).
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m1[124]_", trim_char = "_")
#' df2alpha(df[df$clade == "S", ])
#' if (requireNamespace("ggviolinbox", quietly = TRUE)) {
#'   df2alpha(df[df$clade == "S", ], violinbox = TRUE)
#' }
#'
#' @export
#' @importFrom rlang .data
df2alpha <- function(df, split_by = FALSE, add_legend = FALSE,
                     alpha_function_list = list(Shannon = abdiv::shannon,
                                                Simpson = abdiv::simpson),
                     violinbox = FALSE, box_side = "left",
                     violin_side = "right", ...) {
  df <- as_samovar_df(df)
  gg <- df %>%
    dplyr::group_by(sample) %>%
    dplyr::summarise(dplyr::across(N, alpha_function_list))

  gg <- tidyr::pivot_longer(gg, cols = -1)
  gg$name <- forcats::fct_inorder(factor(stringr::str_remove_all(gg$name, "N_")))

  df2diversity_plot(df, gg, split_by, add_legend, violin = FALSE,
                    violinbox = violinbox, box_side = box_side,
                    violin_side = violin_side, ...)
}

# Shared plotting backend for df2alpha() / df2alpha_summary().
df2diversity_plot <- function(df, gg, split_by, add_legend, violin,
                              violinbox = FALSE, box_side = "left",
                              violin_side = "right", ...) {
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
    p <- p + if (!isFALSE(violinbox)) {
      violinbox_geom(violinbox, box_side, violin_side,
                     mapping = ggplot2::aes(fill = .data$split), alpha = 0.3)
    } else if (violin) {
      ggplot2::geom_violin(trim = FALSE,
                           ggplot2::aes(fill = .data$split), alpha = 0.3)
    } else {
      ggplot2::geom_boxplot(outlier.alpha = 0,
                            ggplot2::aes(fill = .data$split), alpha = 0.3)
    }
    p +
      ggplot2::geom_jitter(ggplot2::aes(color = .data$sample2), width = 0.1) +
      ggplot2::facet_wrap(~name, drop = TRUE, ncol = 4, scales = "free",
                          strip.position = "top") +
      ggplot2::scale_color_discrete(
        NULL, type = viridis::viridis(length(unique(gg$sample2)))) +
      ggplot2::scale_fill_discrete(NULL) +
      ggplot2::ylab("") + ggplot2::xlab("") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "right",
                     axis.text.x = ggplot2::element_blank())
  } else if (!isFALSE(violinbox)) {
    # ggviolinbox needs the categorical variable on x; coord_flip() restores
    # the horizontal (metric-on-x) layout of the classic panel.
    ggplot2::ggplot(gg, ggplot2::aes(x = .data$name, y = .data$value)) +
      violinbox_geom(violinbox, box_side, violin_side) +
      ggplot2::geom_jitter(ggplot2::aes(color = .data$sample2), width = 0.1) +
      ggplot2::coord_flip() +
      ggplot2::facet_wrap(~name, drop = TRUE, ncol = 4, scales = "free",
                          strip.position = "top") +
      ggplot2::scale_color_discrete(
        "", type = viridis::viridis(length(unique(gg$sample))),
        labels = as.character(unlist(leg[, ncol(as.data.frame(leg))]))) +
      ggplot2::ylab("") + ggplot2::xlab("") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "right",
                     axis.text.y = ggplot2::element_blank())
  } else {
    p <- ggplot2::ggplot(gg, ggplot2::aes(x = .data$value, y = .data$name))
    p <- p + if (violin) {
      ggplot2::geom_violin(trim = FALSE)
    } else {
      ggplot2::geom_boxplot(outlier.alpha = 0)
    }
    p +
      ggplot2::geom_jitter(ggplot2::aes(color = .data$sample2), width = 0.1) +
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
#' it as a symmetric [heatmap3::heatmap3] heatmap.
#'
#' @param df A tidy `tibble` from [get_counts()].
#' @param clade Character or `NULL`. Restrict to a single clade.
#' @param dist_function Distance function from \pkg{abdiv}
#'   (default [abdiv::bray_curtis]).
#' @param treshhold_up,treshhold_down Upper / lower mean-abundance thresholds
#'   used to filter taxa.
#' @param add_legend Integer column index/indices (or `FALSE`) drawn as colour
#'   side-bars.
#' @param add_labels Integer column index/indices (or `FALSE`) used for row/col
#'   labels.
#' @param print_df Logical. If `TRUE`, return the distance matrix instead of
#'   drawing.
#' @param ... Passed to [heatmap3::heatmap3].
#'
#' @return Invisibly, the result of [heatmap3::heatmap3] (a heatmap is drawn as
#'   a side effect); or the distance matrix when `print_df = TRUE`.
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
                    print_df = FALSE, ...) {

  df <- as_samovar_df(df)

  name2viridis <- function(name) {
    name <- as.character(unlist(name))
    n_un <- data.frame(name = unique(name),
                       col = viridis::viridis(length(unique(name))))
    name <- dplyr::left_join(data.frame(name = name), n_un, by = "name")
    as.character(unlist(name[, 2]))
  }

  pallete <- grDevices::colorRampPalette(
    c("white", "lightyellow", "orange", "orangered3", "darkred"))(254)
  pallete <- rev(pallete)

  if (!is.null(clade)) df <- df[df$clade %in% clade, ]

  if (!isFALSE(add_legend)) {
    leg <- unique(df[, c(3, add_legend)])
    colsides <- apply(leg[, -1, drop = FALSE], 2, name2viridis)
  } else {
    colsides <- NULL
  }

  if (!isFALSE(add_labels)) {
    leg2 <- unique(df[, c(3, add_labels)])
    leg2 <- apply(leg2[, -1, drop = FALSE], 1,
                  function(z) stringr::str_c(z, collapse = ", "))
  } else {
    leg2 <- as.character(unique(df$sample))
  }

  df_taxa <- dplyr::summarise(df, m = mean(amount_cl), .by = "taxa")
  df_taxa <- df_taxa[df_taxa$m < treshhold_up & df_taxa$m > treshhold_down, ]
  df_taxa <- as.character(unlist(df_taxa[, 1]))

  df <- df[df$taxa %in% df_taxa, ]

  if (print_df) {
    return(df_beta_matrix(df, dist_function))
  }

  draw <- function(df, ...) {
    df2 <- df_beta_matrix(df, dist_function)
    df2[1, 1] <- 1

    heatmap3::heatmap3(
      df2, symm = TRUE,
      col = c("white", pallete, "white"),
      showRowDendro = FALSE,
      labRow = as.expression(lapply(leg2, function(a) bquote(italic(.(a))))),
      labCol = as.expression(lapply(leg2, function(a) bquote(italic(.(a))))),
      method = "ward.D2",
      cexCol = 1.5, cexRow = 1.5, ...)
  }

  if (!is.null(colsides)) {
    invisible(draw(df, ColSideColors = colsides))
  } else {
    invisible(draw(df))
  }
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
                         add_legend = FALSE, add_ellipse = FALSE, ...) {

  df <- as_samovar_df(df)
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

  pcoa_df <- df %>%
    df_untidy(drop_unclassified = TRUE, scale = FALSE) %>%
    as.data.frame() %>%
    t() %>%
    usedist::dist_make(dist_function) %>%
    as.matrix() %>%
    ape::pcoa()

  vectors <- as.data.frame(pcoa_df$vectors)
  vectors$leg1 <- forcats::fct_inorder(factor(leg1))

  gg <- ggplot2::ggplot(vectors, ggplot2::aes(.data$Axis.1, .data$Axis.2)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$leg1)) +
    ggplot2::scale_color_discrete(
      NULL, type = viridis::viridis(length(unique(leg1)))) +
    ggplot2::theme_minimal()

  if (!is.null(leg2)) {
    vectors$leg2 <- leg2
    gg <- gg +
      ggnewscale::new_scale_colour() +
      ggforce::geom_mark_ellipse(
        data = vectors,
        ggplot2::aes(color = .data$leg2, label = .data$leg2),
        label.buffer = ggplot2::unit(-5, "mm"))
  }

  gg
}

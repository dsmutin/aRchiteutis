#' Donut plot of mean taxonomic composition
#'
#' @param df A tidy `tibble` from [get_counts()] (usually already trimmed to a
#'   handful of taxa, e.g. via [df_taxa_trim()]).
#' @param ... Reserved for future use.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m1[124]_", trim_char = "_")
#' df2donut(df_taxa_trim(df[df$clade != "S", ], top_taxa = 6))
#'
#' @export
#' @importFrom rlang .data
df2donut <- function(df, ...) {
  df <- df_tidy_drop_unclassified(df)
  totals <- dplyr::summarise(df, .total = sum(.data$amount), .by = "sample")
  df <- dplyr::left_join(df, totals, by = "sample")
  df$amount <- ifelse(df$.total > 0, df$amount / df$.total, 0)

  df <- dplyr::summarise(df, m = mean(.data$amount), .by = c("taxa", "clade"))
  total <- sum(df$m)
  df$m <- if (is.finite(total) && total > 0) df$m / total else 0
  df <- df[order(df$m), ]
  df$ymax <- cumsum(df$m)
  df$ymin <- c(0, utils::head(df$ymax, n = -1))

  lvir <- length(levels(factor(df$taxa)))

  ggplot2::ggplot(df, ggplot2::aes(ymax = .data$ymax, ymin = .data$ymin,
                                   xmax = 4, xmin = 3, fill = .data$taxa)) +
    ggplot2::geom_rect(color = "white") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(c(2, 4)) +
    ggplot2::scale_fill_discrete("", type = viridis::viridis(lvir)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = ggplot2::unit(0.35, "cm"),
      legend.box.margin = ggplot2::margin(6, 0, 0, 0),
      plot.margin = ggplot2::margin(8, 8, 8, 8)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2))
}

#' Stacked bar plot of composition per sample
#'
#' @param df A tidy `tibble` from [get_counts()].
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m1[124]_", trim_char = "_")
#' df2composition(df_taxa_trim(df[df$clade != "S", ], top_taxa = 6))
#'
#' @export
#' @importFrom rlang .data
df2composition <- function(df) {
  df <- df_tidy_drop_unclassified(df)
  totals <- dplyr::summarise(df, .total = sum(.data$amount), .by = "sample")
  df <- dplyr::left_join(df, totals, by = "sample")
  df$amount <- ifelse(df$.total > 0, df$amount / df$.total, 0)
  df$.total <- NULL

  lvir <- length(levels(factor(df$taxa)))

  ggplot2::ggplot(df, ggplot2::aes(y = .data$sample, x = .data$amount,
                                   fill = forcats::fct_inorder(.data$taxa))) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::scale_fill_discrete("Taxa", type = rev(viridis::viridis(lvir))) +
    ggplot2::scale_x_continuous(limits = c(0, 1),
                                expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = ggplot2::unit(0.35, "cm"),
      axis.text.y = ggplot2::element_text(size = 8),
      plot.margin = ggplot2::margin(8, 16, 8, 8)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2))
}

#' Box plot of per-sample composition across taxa
#'
#' @param df A tidy `tibble` from [get_counts()].
#' @param style `"box"` or `"raincloud"` (half-violin and half-box via
#'   \pkg{ggviolinbox}, same fill as the box).
#' @param ... Reserved for future use.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m1[124]_", trim_char = "_")
#' df2barplot(df_taxa_trim(df[df$clade != "S", ], top_taxa = 6))
#' if (requireNamespace("ggviolinbox", quietly = TRUE)) {
#'   df2barplot(df_taxa_trim(df[df$clade != "S", ], top_taxa = 6),
#'              style = "raincloud")
#' }
#'
#' @export
#' @importFrom rlang .data
df2barplot <- function(df, ..., style = c("box", "raincloud")) {
  style <- match.arg(style)
  lvir <- length(levels(droplevels(factor(df$taxa))))

  df_sum <- dplyr::summarise(df, m = mean(amount), .by = "taxa")
  df <- dplyr::left_join(df, df_sum, by = "taxa")

  df <- df[order(df$m), ]

  taxa <- stringr::str_detect(df$taxa, "other")
  df <- rbind(df[taxa, ], df[!taxa, ])
  df$taxa <- forcats::fct_inorder(df$taxa)

  pos <- df$amount[df$amount > 0]
  floor_val <- if (length(pos)) min(pos) else 0
  df$amount_log <- log10(df$amount + floor_val)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$amount_log, y = .data$taxa,
                                       fill = .data$taxa))
  if (identical(style, "raincloud")) {
    p <- p + archi_rain_layers(
      ggplot2::aes(fill = .data$taxa), orientation = "y"
    )
  } else {
    p <- p + ggplot2::geom_boxplot(show.legend = FALSE)
  }
  p +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::xlab(expression(log[10](x + min(x[x > 0])))) +
    ggplot2::ylab("") +
    ggplot2::scale_fill_discrete("", type = viridis::viridis(lvir)) +
    ggplot2::guides(fill = "none") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 8, face = "italic"),
      plot.margin = ggplot2::margin(8, 12, 8, 8))
}

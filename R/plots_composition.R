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

  df <- dplyr::summarise(df, m = mean(amount), .by = c("taxa", "clade"))
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

  lvir <- length(levels(factor(df$taxa)))

  ggplot2::ggplot(df, ggplot2::aes(y = .data$sample, x = .data$amount,
                                   fill = forcats::fct_inorder(.data$taxa))) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::scale_fill_discrete("Taxa", type = rev(viridis::viridis(lvir))) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.02))) +
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
#' @param ... Reserved for future use.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m1[124]_", trim_char = "_")
#' df2barplot(df_taxa_trim(df[df$clade != "S", ], top_taxa = 6))
#'
#' @export
#' @importFrom rlang .data
df2barplot <- function(df, ...) {
  lvir <- length(levels(droplevels(factor(df$taxa))))

  df_sum <- dplyr::summarise(df, m = mean(amount), .by = "taxa")
  df <- dplyr::left_join(df, df_sum, by = "taxa")

  df <- df[order(df$m), ]

  taxa <- stringr::str_detect(df$taxa, "other")
  df <- rbind(df[taxa, ], df[!taxa, ])
  df$taxa <- forcats::fct_inorder(df$taxa)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$amount, y = .data$taxa,
                                   fill = .data$taxa)) +
    ggplot2::geom_boxplot(show.legend = FALSE) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::scale_fill_discrete("", type = viridis::viridis(lvir)) +
    ggplot2::guides(fill = "none") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 8),
      plot.margin = ggplot2::margin(8, 12, 8, 8))
}

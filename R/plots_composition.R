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

  df <- as_samovar_df(df)
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
    ggplot2::theme_void()
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

  df <- as_samovar_df(df)
  lvir <- length(levels(factor(df$taxa)))

  ggplot2::ggplot(df, ggplot2::aes(y = .data$sample, x = .data$amount,
                                   fill = forcats::fct_inorder(.data$taxa))) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::scale_fill_discrete("Taxa", type = rev(viridis::viridis(lvir))) +
    ggplot2::theme_minimal()
}

#' Box plot of per-sample composition across taxa
#'
#' @param df A tidy `tibble` from [get_counts()].
#' @param violinbox Controls the per-taxon distribution geom. `FALSE` (default)
#'   keeps the classic horizontal \pkg{ggplot2} `geom_boxplot`. `TRUE` or
#'   `"combined"` draws a combined half-violin + half-boxplot via
#'   [ggviolinbox::geom_violinboxplot()]; `"halves"` uses a
#'   [ggviolinbox::geom_halfviolin()] + [ggviolinbox::geom_halfboxplot()] pair.
#'   Because ggviolinbox needs the categorical variable on the x axis, the
#'   violinbox layout maps taxa to x and applies [ggplot2::coord_flip()] to keep
#'   the familiar horizontal orientation. Any non-`FALSE` value requires the
#'   \pkg{ggviolinbox} package.
#' @param box_side,violin_side Side (`"left"` / `"right"`) each geom is drawn on
#'   when `violinbox` is enabled. Ignored when `violinbox = FALSE`.
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
#'              violinbox = TRUE)
#' }
#'
#' @export
#' @importFrom rlang .data
df2barplot <- function(df, violinbox = FALSE, box_side = "left",
                       violin_side = "right", ...) {
  df <- as_samovar_df(df)
  lvir <- length(levels(droplevels(factor(df$taxa))))

  df_sum <- dplyr::summarise(df, m = mean(amount), .by = "taxa")
  df <- dplyr::left_join(df, df_sum, by = "taxa")

  df <- df[order(df$m), ]

  taxa <- stringr::str_detect(df$taxa, "other")
  df <- rbind(df[taxa, ], df[!taxa, ])
  df$taxa <- forcats::fct_inorder(df$taxa)

  if (!isFALSE(violinbox)) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$taxa, y = .data$amount,
                                          fill = .data$taxa)) +
      violinbox_geom(violinbox, box_side, violin_side, show.legend = FALSE) +
      ggplot2::coord_flip()
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$amount, y = .data$taxa,
                                          fill = .data$taxa)) +
      ggplot2::geom_boxplot(show.legend = FALSE)
  }

  p +
    ggplot2::theme_minimal() +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::scale_fill_discrete("", type = viridis::viridis(lvir)) +
    ggplot2::theme(text = ggplot2::element_text(size = 20))
}

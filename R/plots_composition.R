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

#' Order composition-bar samples
#'
#' `"fpc"` sorts by the first principal component of the sample-by-taxon
#' matrix (the same score order the harness uses for adjacency matrices).
#' `"hclust"` follows average-linkage Bray-Curtis. `"abundance"` and
#' `"alpha"` sort by total amount and Shannon. `"none"` keeps input order.
#'
#' @keywords internal
archi_order_sample_levels <- function(df, method = "fpc") {
  method <- match.arg(method, c("fpc", "hclust", "abundance", "alpha", "none"))
  samples <- unique(as.character(df$sample))
  if (length(samples) < 2L || identical(method, "none")) return(samples)
  mat <- df_untidy(df, amount_from = "amount")
  mat <- mat[, intersect(samples, colnames(mat)), drop = FALSE]
  if (ncol(mat) < 2L) return(samples)
  ord <- if (identical(method, "fpc")) {
    X <- t(mat)
    X <- X[, colSums(X) > 0, drop = FALSE]
    if (ncol(X) < 2L) {
      colnames(mat)
    } else {
      pc <- stats::prcomp(X, center = TRUE, scale. = FALSE)
      rownames(pc$x)[order(pc$x[, 1])]
    }
  } else if (identical(method, "hclust")) {
    d <- usedist::dist_make(as.data.frame(t(mat)), abdiv::bray_curtis)
    colnames(mat)[stats::hclust(d, method = "average")$order]
  } else if (identical(method, "abundance")) {
    names(sort(colSums(mat), decreasing = TRUE))
  } else {
    names(sort(apply(mat, 2, abdiv::shannon), decreasing = TRUE))
  }
  c(ord, setdiff(samples, ord))
}

#' Stacked bar plot of composition per sample
#'
#' Samples are ordered by the first principal component of the composition
#' matrix (`order_samples = "fpc"`). Other orders: `"hclust"`, `"abundance"`,
#' `"alpha"`, `"none"`.
#'
#' @param df A tidy `tibble` from [get_counts()].
#' @param order_samples Sample order. See Details.
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
df2composition <- function(df, order_samples = c("fpc", "hclust", "abundance", "alpha", "none")) {
  order_samples <- match.arg(order_samples)
  df <- df_tidy_drop_unclassified(df)
  totals <- dplyr::summarise(df, .total = sum(.data$amount), .by = "sample")
  df <- dplyr::left_join(df, totals, by = "sample")
  df$amount <- ifelse(df$.total > 0, df$amount / df$.total, 0)
  df$.total <- NULL
  df$sample <- factor(df$sample, levels = archi_order_sample_levels(df, order_samples))

  lvir <- length(levels(factor(df$taxa)))

  ggplot2::ggplot(df, ggplot2::aes(y = .data$sample, x = .data$amount,
                                   fill = forcats::fct_inorder(.data$taxa))) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::scale_fill_discrete("Taxa", type = rev(viridis::viridis(lvir))) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1)) +
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

#' Taxonomic composition on a fan tree
#'
#' A fan tree drawn with \pkg{ggtree}, tip colour marking a higher rank, and a
#' \pkg{ggtreeExtra} boxplot ring of per-sample relative abundance. The layout
#' follows the ggtreeExtra fruit-boxplot example: `layout = "fan"`,
#' `open.angle = 10`, [ggtree::rotate_tree()], then [ggtreeExtra::geom_fruit]
#' with [ggplot2::geom_boxplot] (`y` is the tip, `group` is the tip label,
#' `fill` is the rank).
#'
#' @param df A tidy table from [get_counts()].
#' @param tax Optional rank table with a `taxa` column (or row names) matching
#'   `df$taxa`, plus Linnaean columns. When `NULL`, genus and species are
#'   parsed from the labels and tips are coloured by genus.
#' @param color_rank Rank used to colour tips and boxes. `"phylum"` when that
#'   column exists, otherwise genus.
#' @param top Integer. Most abundant taxa to keep.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @references
#' Yu G, Smith DK, Zhu H, Guan Y, Lam TTY (2017). "ggtree: an R package for
#' visualization and annotation of phylogenetic trees with their covariates
#' and other associated data." *Methods in Ecology and Evolution*, 8, 28–36.
#' \doi{10.1111/2041-210X.12628}
#'
#' Xu S, Dai Z, Guo P, Fu X, Liu S, Zhou L, Tang W, Feng T, Chen M, Zhan L,
#' Wu T, Hu E, Jiang Y, Bo X, Yu G (2021). "ggtreeExtra: Compact Visualization
#' of Richly Annotated Phylogenetic Data." *Molecular Biology and Evolution*,
#' 38, 4039–4042. \doi{10.1093/molbev/msab166}
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m1[124]_", trim_char = "_")
#' df2composition_tree(df[df$clade == "G", ], top = 12)
#'
#' @export
#' @importFrom rlang .data
df2composition_tree <- function(df, tax = NULL, color_rank = "phylum", top = 25L) {
  if (is.null(df)) stop("df2composition_tree needs a tidy table", call. = FALSE)
  df <- df_tidy_drop_unclassified(df)
  means <- dplyr::summarise(df, m = mean(.data$amount), .by = "taxa")
  means <- means[order(-means$m), , drop = FALSE]
  means <- utils::head(means, min(as.integer(top), nrow(means)))
  if (nrow(means) < 2L) stop("df2composition_tree needs at least two taxa", call. = FALSE)
  df <- df[df$taxa %in% means$taxa, , drop = FALSE]
  built <- archi_composition_tree(df, tax, color_rank)
  archi_prepare_ggtree()
  # Same layout as the ggtreeExtra fruit-boxplot example: fan, tip colour by
  # a higher rank, then a boxplot ring of per-sample abundance.
  rank_col <- built$rank_title
  meta <- built$meta
  names(meta)[names(meta) == "rank_color"] <- rank_col
  p <- ggtree::ggtree(built$tree, layout = "fan", open.angle = 10)
  p <- ggtree::`%<+%`(p, meta)
  p <- p + ggtree::geom_tippoint(
    ggplot2::aes(color = .data[[rank_col]]), size = 1.5, show.legend = FALSE
  )
  p <- suppressMessages(ggtree::rotate_tree(p, -90))
  boxes <- data.frame(
    OTU = as.character(df$taxa),
    val = as.numeric(df$amount) * 100,
    stringsAsFactors = FALSE
  )
  boxes <- boxes[boxes$OTU %in% built$tree$tip.label & is.finite(boxes$val), ,
                 drop = FALSE]
  # geom_fruit joins on the tree column `label` and drops that name. A data
  # function keeps `label` so group = label is one box per tip, and the rank
  # column (Phylum in the example) stays available for fill.
  fruit_data <- function(plot_data) {
    tips <- plot_data[plot_data$isTip, , drop = FALSE]
    out <- merge(boxes, tips, by.x = "OTU", by.y = "label")
    out$label <- out$OTU
    out
  }
  legend_name <- if (identical(rank_col, "Phylum")) "Phyla" else rank_col
  p +
    archi_geom_fruit(
      data = fruit_data,
      mapping = rlang::inject(ggplot2::aes(
        y = OTU,
        x = val,
        group = label,
        fill = !!rlang::sym(rank_col)
      )),
      offset = 0.03,
      pwidth = 0.2,
      geom = ggplot2::geom_boxplot,
      geomname = "geom_boxplot",
      size = 0.2,
      outlier.size = 0.5,
      outlier.stroke = 0.08,
      outlier.shape = 21
    ) +
    ggplot2::scale_fill_discrete(
      name = legend_name,
      guide = ggplot2::guide_legend(keywidth = 0.8, keyheight = 0.8, ncol = 1)
    ) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 7)
    )
}

#' Fan-tree tips and the rank used to colour them
#' @keywords internal
archi_composition_tree <- function(df, tax, color_rank) {
  labels <- unique(as.character(df$taxa))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (is.null(tax)) {
    genus <- ifelse(grepl(" ", labels), sub(" .*", "", labels), labels)
    meta <- data.frame(
      label = labels, rank_color = genus, stringsAsFactors = FALSE
    )
    return(list(tree = archi_label_tree(labels), meta = meta, rank_title = "Genus"))
  }
  tax <- as.data.frame(tax, stringsAsFactors = FALSE)
  if (!"taxa" %in% names(tax)) tax$taxa <- rownames(tax)
  tax$taxa <- as.character(tax$taxa)
  names(tax) <- tolower(names(tax))
  tax <- tax[tax$taxa %in% labels, , drop = FALSE]
  ranks <- intersect(archi_rank_cols(), names(tax))
  color_rank <- tolower(color_rank)
  if (!color_rank %in% names(tax) || all(is.na(tax[[color_rank]]) | !nzchar(tax[[color_rank]]))) {
    color_rank <- if ("genus" %in% names(tax)) "genus" else ranks[[length(ranks)]]
  }
  if (length(ranks) >= 2L && nrow(tax) >= 2L) {
    tax$tip_name <- tax$taxa
    tree <- ranks_to_tree(tax)
  } else {
    tree <- archi_label_tree(intersect(labels, tax$taxa))
  }
  color <- as.character(tax[[color_rank]][match(tree$tip.label, tax$taxa)])
  missing <- is.na(color) | !nzchar(color)
  color[missing] <- ifelse(
    grepl(" ", tree$tip.label[missing]),
    sub(" .*", "", tree$tip.label[missing]),
    tree$tip.label[missing]
  )
  meta <- data.frame(label = tree$tip.label, rank_color = color, stringsAsFactors = FALSE)
  title <- paste0(toupper(substr(color_rank, 1, 1)), substr(color_rank, 2, nchar(color_rank)))
  list(tree = tree, meta = meta, rank_title = title)
}

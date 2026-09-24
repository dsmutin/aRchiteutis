# Visualisations adapted from the metagenomics harness
# (rarefaction curves, ggtree differential trees, heat trees, UpSet).
# CLI wrappers and hooks stay out of the package; only the tested plotting
# and matrix logic is reused.

#' Rarefy one count vector without replacement
#'
#' Same draw as phyloseq::rarefy_even_depth / vegan::rrarefy: a simple random
#' sample of `depth` observations from the multinomial counts.
#'
#' @param counts Numeric vector of counts.
#' @param depth Integer sequencing depth.
#' @return Integer vector the same length as `counts`, or `NULL` when
#'   `sum(counts) < depth`.
#' @keywords internal
archi_rarefy_counts <- function(counts, depth) {
  counts <- as.numeric(counts)
  counts[!is.finite(counts) | counts < 0] <- 0
  counts <- round(counts)
  total <- sum(counts)
  depth <- as.integer(depth)
  if (!length(counts) || total < depth || depth < 1L) return(NULL)
  if (total == depth) return(as.integer(counts))
  pool <- rep.int(seq_along(counts), counts)
  picked <- sample(pool, depth, replace = FALSE)
  as.integer(tabulate(picked, nbins = length(counts)))
}

#' Depth grid used for alpha rarefaction curves
#'
#' A short form of the harness grid (1, 5, 10, … up to the smallest library),
#' so curves stay readable on classifier reports.
#'
#' @param totals Numeric vector of per-sample read totals.
#' @return Integer vector of depths.
#' @keywords internal
archi_rarefaction_depths <- function(totals) {
  totals <- totals[is.finite(totals) & totals > 0]
  max_sum <- if (length(totals)) as.integer(min(totals)) else 0L
  if (!is.finite(max_sum) || max_sum < 1L) {
    stop("Rarefaction needs at least one sample with reads", call. = FALSE)
  }
  grid <- unique(as.integer(c(
    1L, 5L, 10L, 20L, 50L, 100L,
    if (max_sum >= 200L) seq(200L, min(2000L, max_sum), by = 200L) else integer(0),
    if (max_sum >= 2500L) seq(2500L, max_sum, by = 500L) else integer(0),
    max_sum
  )))
  grid[grid > 0L & grid <= max_sum]
}

#' Alpha-diversity rarefaction curves
#'
#' At each depth and replicate, each sample with enough reads is rarefied and
#' Observed / Shannon / Simpson are recorded. The plot follows the harness
#' grazing figure: one thin line per sample, a loess smooth by group, and a
#' dashed line at the depths that still include every plotted sample.
#'
#' @param df A tidy table from [get_counts()]. Species rows are appropriate.
#' @param split_by Integer column index or column name used to colour samples.
#'   `FALSE` colours by sample.
#' @param measures Character vector: any of `"Observed"`, `"Shannon"`,
#'   `"Simpson"`.
#' @param depths Integer depths. `NULL` uses a short grid up to the smallest library.
#' @param n_reps Integer. Rarefaction replicates per depth.
#' @param seed Integer. RNG seed.
#' @param top Integer or `FALSE`. Keep the `top` most abundant taxa before
#'   rarefying (faster on full Kraken2 reports).
#'
#' @return A [ggplot2::ggplot] object. The long replicate table is attached as
#'   attribute `"rarefaction"`.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
#' df <- get_counts(path, pattern = "m1[12]_", legend = legend, trim_char = "_")
#' df2rarefaction(df[df$clade == "S", ], split_by = "stage",
#'                depths = c(20L, 50L), n_reps = 2L, top = 25L)
#'
#' @export
#' @importFrom rlang .data
df2rarefaction <- function(df, split_by = FALSE,
                           measures = c("Observed", "Shannon", "Simpson"),
                           depths = NULL, n_reps = 3L, seed = 123L,
                           top = FALSE) {
  measures <- match.arg(measures, several.ok = TRUE)
  mat <- df_untidy(df, amount_from = "N", top = top, drop_unclassified = TRUE)
  if (nrow(mat) < 1L || ncol(mat) < 1L) {
    stop("Rarefaction needs taxa and samples", call. = FALSE)
  }
  totals <- colSums(mat)
  if (is.null(depths)) depths <- archi_rarefaction_depths(totals)
  depths <- sort(unique(as.integer(depths)))
  depths <- depths[depths > 0L]
  if (!length(depths)) stop("No valid rarefaction depths", call. = FALSE)
  n_reps <- max(1L, as.integer(n_reps))

  group <- archi_sample_group(df, split_by, colnames(mat))
  rows <- vector("list", length(depths) * n_reps * ncol(mat) * length(measures))
  k <- 0L
  for (d in depths) {
    keep <- which(totals >= d)
    if (!length(keep)) next
    for (r in seq_len(n_reps)) {
      set.seed(as.integer(seed) + as.integer(d) * 1000L + as.integer(r))
      for (j in keep) {
        rare <- archi_rarefy_counts(mat[, j], d)
        if (is.null(rare)) next
        vals <- c(
          Observed = sum(rare > 0),
          Shannon = abdiv::shannon(rare),
          Simpson = abdiv::simpson(rare)
        )
        for (m in measures) {
          k <- k + 1L
          rows[[k]] <- data.frame(
            Sample = colnames(mat)[j],
            Measure = m,
            Depth = d,
            Rep = r,
            value = unname(vals[[m]]),
            target = unname(group[[colnames(mat)[j]]]),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  rows <- rows[seq_len(k)]
  if (!length(rows)) stop("Alpha rarefaction produced no rows", call. = FALSE)
  long_df <- do.call(rbind, rows)
  sample_df <- stats::aggregate(
    value ~ Sample + Measure + Depth + target,
    data = long_df,
    FUN = mean
  )
  covered <- tapply(sample_df$Sample, sample_df$Depth, function(s) {
    length(unique(s)) == ncol(mat)
  })
  common_depths <- as.integer(names(covered)[covered])
  vline <- if (length(common_depths)) max(common_depths) else min(sample_df$Depth)

  sample_df$Measure <- factor(sample_df$Measure, levels = measures)
  p <- ggplot2::ggplot(
    sample_df,
    ggplot2::aes(x = .data$Depth, y = .data$value,
                 colour = .data$target, fill = .data$target)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(group = .data$Sample),
      alpha = 0.35, linewidth = 0.35
    )
  # Loess needs several depths (harness geom_smooth). A short grid uses a line.
  if (length(unique(sample_df$Depth)) >= 4L) {
    p <- p + ggplot2::geom_smooth(
      ggplot2::aes(group = .data$target),
      method = "loess", formula = y ~ x, se = TRUE,
      alpha = 0.12, linewidth = 0.9
    )
  } else {
    p <- p + ggplot2::stat_summary(
      ggplot2::aes(group = .data$target),
      fun = mean, geom = "line", linewidth = 0.9
    )
  }
  p <- p +
    ggplot2::geom_vline(
      xintercept = vline, linetype = "dashed",
      linewidth = 0.4, colour = "grey30"
    ) +
    ggplot2::facet_wrap(~Measure, scales = "free_y") +
    ggplot2::labs(
      x = "Sequencing depth",
      y = "Diversity index",
      colour = if (isFALSE(split_by)) "sample" else "group",
      fill = if (isFALSE(split_by)) "sample" else "group"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(legend.position = "right")
  attr(p, "rarefaction") <- long_df
  p
}

#' Map a tidy-table column onto samples
#' @keywords internal
archi_sample_group <- function(df, split_by, samples) {
  samples <- as.character(samples)
  if (isFALSE(split_by) || is.null(split_by)) {
    return(stats::setNames(samples, samples))
  }
  if (is.character(split_by) && length(split_by) == 1L && split_by %in% names(df)) {
    leg <- unique(df[, c("sample", split_by)])
    vals <- as.character(leg[[2]])
  } else {
    idx <- as.integer(split_by)
    leg <- unique(df[, c(3, idx), drop = FALSE])
    vals <- apply(leg[, -1, drop = FALSE], 1, function(z) paste(z, collapse = ", "))
  }
  names(vals) <- as.character(leg[[1]])
  missing <- setdiff(samples, names(vals))
  if (length(missing)) vals[missing] <- missing
  vals[samples]
}

#' Two-group log2 fold change of mean relative abundance
#' @keywords internal
archi_group_lfc <- function(df, group, contrast = NULL) {
  df <- df_tidy_drop_unclassified(df)
  g <- archi_sample_group(df, group, unique(as.character(df$sample)))
  df$group <- unname(g[as.character(df$sample)])
  lev <- unique(df$group)
  if (!is.null(contrast)) {
    if (length(contrast) != 2L || !all(contrast %in% lev)) {
      stop("contrast must be two group levels present in the table", call. = FALSE)
    }
    lev <- contrast
  } else if (length(lev) < 2L) {
    stop("df2difftree needs a grouping column with at least two levels", call. = FALSE)
  } else if (length(lev) > 2L) {
    stop(
      "df2difftree found more than two groups; choose exactly two with `contrast`",
      call. = FALSE
    )
  } else {
    lev <- lev
  }
  sub <- df[df$group %in% lev, , drop = FALSE]
  means <- dplyr::summarise(sub, m = mean(.data$amount), .by = c("taxa", "group"))
  wide <- tidyr::pivot_wider(means, names_from = "group", values_from = "m", values_fill = 0)
  a <- wide[[lev[1]]]
  b <- wide[[lev[2]]]
  eps <- 1e-6
  data.frame(
    id = as.character(wide$taxa),
    log2_lfc = log2((b + eps) / (a + eps)),
    group_low = lev[1],
    group_high = lev[2],
    stringsAsFactors = FALSE
  )
}

#' Rank-formula tree for a set of binomial (or single-word) tip labels
#'
#' Uses the harness rule: `ape::as.phylo(~genus/species)` with factors, not an
#' `hclust` dendrogram.
#'
#' @param labels Character tip labels.
#' @return An [ape::phylo] object.
#' @keywords internal
archi_label_tree <- function(labels) {
  labels <- unique(as.character(labels))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  genus <- ifelse(grepl(" ", labels), sub(" .*", "", labels), labels)
  species <- labels
  dup <- duplicated(species) | duplicated(species, fromLast = TRUE)
  if (any(dup)) species[dup] <- paste0(species[dup], " [", seq_len(sum(dup)), "]")
  tax <- data.frame(genus = factor(genus), species = factor(species))
  tr <- ape::as.phylo(data = tax, ~ genus / species)
  # Tip labels from as.phylo are the species factor levels.
  tr
}

#' Rectangular layout for a phylo object (tips evenly spaced)
#' @keywords internal
archi_phylo_layout <- function(tree) {
  tree <- ape::reorder.phylo(tree, order = "cladewise")
  tips <- tree$tip.label
  y <- stats::setNames(seq_along(tips), tips)
  n_node <- length(tips) + tree$Nnode
  yy <- numeric(n_node)
  yy[seq_along(tips)] <- y
  children <- split(tree$edge[, 2], tree$edge[, 1])
  for (node in seq.int(n_node, length(tips) + 1L)) {
    ch <- children[[as.character(node)]]
    if (is.null(ch)) next
    yy[node] <- mean(yy[ch])
  }
  xx <- numeric(n_node)
  for (i in seq_len(nrow(tree$edge))) {
    parent <- tree$edge[i, 1]
    child <- tree$edge[i, 2]
    xx[child] <- xx[parent] + 1
  }
  nodes <- data.frame(
    node = seq_len(n_node),
    x = xx,
    y = yy,
    label = c(tips, rep(NA_character_, tree$Nnode)),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    x = xx[tree$edge[, 1]],
    xend = xx[tree$edge[, 2]],
    y = yy[tree$edge[, 1]],
    yend = yy[tree$edge[, 2]],
    stringsAsFactors = FALSE
  )
  list(nodes = nodes, edges = edges, tips = tips)
}

#' Differential abundance tree (ggtree when installed)
#'
#' Selects the taxa with the largest absolute log2 fold change between two
#' groups and draws them on a rank-formula tree. When \pkg{ggtree} is installed
#' the tree uses that layout (circular or rectangular, branch lengths dropped)
#' and the log2 fold change is a bar at each tip. Otherwise a rectangular
#' cladogram plus the same bar is drawn with \pkg{ggplot2}.
#'
#' @param df A tidy table from [get_counts()].
#' @param group Column index or name with at least two levels (for example the
#'   legend column `stage`).
#' @param contrast Character vector of length 2 naming the groups. The fold
#'   change is `log2(contrast[2] / contrast[1])`.
#' @param max_tips Integer. How many taxa to keep.
#' @param layout `"rectangular"` or `"circular"` (circular needs \pkg{ggtree}).
#' @param tree Optional [ape::phylo]. Tips must match `taxa` values. When
#'   `NULL`, a genus/species formula tree is built from the labels.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
#' df <- get_counts(path, pattern = "m(11|18|4)_", legend = legend, trim_char = "_")
#' df2difftree(df[df$clade == "S", ], group = "stage", max_tips = 12)
#'
#' @export
#' @importFrom rlang .data
df2difftree <- function(df, group, contrast = NULL, max_tips = 20L,
                        layout = c("rectangular", "circular"),
                        tree = NULL) {
  layout <- match.arg(layout)
  lfc <- archi_group_lfc(df, group, contrast = contrast)
  lfc$abs_lfc <- abs(lfc$log2_lfc)
  lfc <- lfc[order(-lfc$abs_lfc), , drop = FALSE]
  lfc <- utils::head(lfc, min(as.integer(max_tips), nrow(lfc)))
  if (nrow(lfc) < 2L) stop("Need at least two taxa for a differential tree", call. = FALSE)

  if (is.null(tree)) {
    tree <- archi_label_tree(lfc$id)
  } else {
    tree <- ape::keep.tip(tree, intersect(tree$tip.label, lfc$id))
  }
  keep <- intersect(lfc$id, tree$tip.label)
  lfc <- lfc[match(keep, lfc$id), , drop = FALSE]
  if (nrow(lfc) < 2L) stop("Tree and taxa share fewer than two tips", call. = FALSE)
  tree <- ape::keep.tip(tree, lfc$id)

  use_ggtree <- requireNamespace("ggtree", quietly = TRUE)
  if (identical(layout, "circular") && !use_ggtree) {
    stop("Circular df2difftree needs ggtree", call. = FALSE)
  }
  if (use_ggtree) {
    return(archi_difftree_ggtree(tree, lfc, layout))
  }
  archi_difftree_ggplot(tree, lfc)
}

#' ggtree differential tree with a fold-change bar at each tip
#' @keywords internal
archi_difftree_ggtree <- function(tree, lfc, layout) {
  lim <- max(abs(lfc$log2_lfc), na.rm = TRUE)
  if (!is.finite(lim) || lim <= 0) lim <- 1
  is_circ <- identical(layout, "circular")
  if (is_circ) {
    p <- ggtree::ggtree(tree, layout = "circular", open.angle = 10,
                        branch.length = "none")
  } else {
    p <- ggtree::ggtree(tree, layout = "rectangular", branch.length = "none")
  }
  tips <- p$data[!is.na(p$data$label) & p$data$label %in% lfc$id, , drop = FALSE]
  tips$log2_lfc <- lfc$log2_lfc[match(tips$label, lfc$id)]
  tips <- tips[!is.na(tips$log2_lfc), , drop = FALSE]
  xmax <- max(p$data$x, na.rm = TRUE)
  span <- if (is_circ) 0.35 else 0.8
  gap <- if (is_circ) 0.2 else 0.45
  tips$xbar <- xmax + gap
  tips$xend <- tips$xbar + tips$log2_lfc / lim * span
  p +
    ggtree::geom_tiplab(size = 2.2, offset = gap + span + 0.15, fontface = "italic") +
    ggplot2::geom_segment(
      data = tips,
      ggplot2::aes(x = .data$xbar, xend = .data$xend, y = .data$y, yend = .data$y,
                   colour = .data$log2_lfc),
      inherit.aes = FALSE, linewidth = 2.2, lineend = "butt"
    ) +
    ggplot2::scale_colour_gradient2(
      low = "#3B4CC0", mid = "grey80", high = "#B40426",
      midpoint = 0, limits = c(-lim, lim), name = "log2 LFC"
    ) +
    ggplot2::theme(legend.position = "right")
}

#' Rectangular differential tree without ggtree
#' @keywords internal
archi_difftree_ggplot <- function(tree, lfc) {
  lay <- archi_phylo_layout(tree)
  tips <- lay$nodes[!is.na(lay$nodes$label), , drop = FALSE]
  tips$log2_lfc <- lfc$log2_lfc[match(tips$label, lfc$id)]
  lim <- max(abs(tips$log2_lfc), na.rm = TRUE)
  if (!is.finite(lim) || lim <= 0) lim <- 1
  xmax <- max(lay$nodes$x)
  # Place the fold-change bar just to the right of the tips.
  bar_x0 <- xmax + 0.6
  tips$xbar <- bar_x0
  tips$xend <- bar_x0 + tips$log2_lfc / lim
  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = lay$edges,
      ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
      linewidth = 0.3, colour = "grey20"
    ) +
    ggplot2::geom_segment(
      data = tips,
      ggplot2::aes(x = .data$xbar, xend = .data$xend, y = .data$y, yend = .data$y,
                   colour = .data$log2_lfc),
      linewidth = 2.4, lineend = "butt"
    ) +
    ggplot2::geom_text(
      data = tips,
      ggplot2::aes(x = .data$x + 0.08, y = .data$y, label = .data$label),
      hjust = 0, size = 2.6, fontface = "italic"
    ) +
    ggplot2::scale_colour_gradient2(
      low = "#3B4CC0", mid = "grey90", high = "#B40426",
      midpoint = 0, limits = c(-lim, lim), name = "log2 LFC"
    ) +
    ggplot2::labs(
      subtitle = paste0("log2(", lfc$group_high[1], " / ", lfc$group_low[1], ")")
    ) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.margin = ggplot2::margin(8, 16, 8, 8)
    )
}

#' Presence / absence UpSet bars across groups
#'
#' A taxon is present in a group when it has a positive count in any sample of
#' that group (the harness fallback when MicrobiotaProcess is not used). Bars
#' show intersection sizes; the matrix underneath marks which groups belong to
#' each intersection.
#'
#' @param df A tidy table from [get_counts()].
#' @param group Column index or name.
#' @param min_size Integer. Drop intersections smaller than this.
#'
#' @return A [ggplot2::ggplot] object (a patchwork-free single plot with the
#'   combination matrix encoded as points under the bars).
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
#' df <- get_counts(path, pattern = "decont_b", legend = legend, trim_char = "_")
#' df2upset(df[df$clade == "G", ], group = "stage")
#'
#' @export
#' @importFrom rlang .data
df2upset <- function(df, group, min_size = 1L) {
  df <- df_tidy_drop_unclassified(df)
  samples <- unique(as.character(df$sample))
  g <- archi_sample_group(df, group, samples)
  df$group <- unname(g[as.character(df$sample)])
  sets <- unique(df$group)
  if (length(sets) < 2L) stop("df2upset needs at least two groups", call. = FALSE)
  mat <- df_untidy(df, amount_from = "N", drop_unclassified = TRUE)
  present <- sapply(sets, function(lv) {
    samps <- intersect(names(g)[g == lv], colnames(mat))
    if (!length(samps)) return(rep(0L, nrow(mat)))
    as.integer(rowSums(mat[, samps, drop = FALSE] > 0) > 0)
  })
  colnames(present) <- sets
  rownames(present) <- rownames(mat)
  key <- apply(present, 1, function(z) paste(z, collapse = ""))
  tab <- as.data.frame(table(key), stringsAsFactors = FALSE)
  names(tab) <- c("key", "size")
  tab <- tab[tab$size >= as.integer(min_size) & grepl("1", tab$key), , drop = FALSE]
  tab <- tab[order(-tab$size), , drop = FALSE]
  if (!nrow(tab)) stop("No shared or private taxa at this min_size", call. = FALSE)
  tab$combo <- factor(tab$key, levels = rev(tab$key))
  bits <- do.call(rbind, strsplit(tab$key, ""))
  storage.mode(bits) <- "integer"
  colnames(bits) <- sets
  long <- cbind(combo = tab$combo, as.data.frame(bits), size = tab$size)
  long <- tidyr::pivot_longer(long, cols = dplyr::all_of(sets),
                              names_to = "set", values_to = "in_set")
  long <- long[long$in_set == 1L, , drop = FALSE]
  ggplot2::ggplot(tab, ggplot2::aes(x = .data$size, y = .data$combo)) +
    ggplot2::geom_col(fill = "grey25", width = 0.7) +
    ggplot2::geom_point(
      data = long,
      ggplot2::aes(x = max(tab$size) * 1.08, y = .data$combo, colour = .data$set),
      size = 2.4, inherit.aes = FALSE
    ) +
    ggplot2::scale_colour_manual(
      values = stats::setNames(viridis::viridis(length(sets)), sets),
      name = NULL
    ) +
    ggplot2::labs(x = "Taxa in intersection", y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank())
}

#' Taxonomic heat tree
#'
#' Builds a rank tree and colours nodes by mean relative abundance. When
#' \pkg{metacoder} is installed and `tax` contains Linnaean rank columns, the
#' harness `metacoder::heat_tree` layout is used. Otherwise nodes of a
#' genus/species formula tree are drawn with \pkg{ggplot2} (size = abundance).
#'
#' @param df A tidy table from [get_counts()], or ignored when `tax` is given.
#' @param tax Optional data frame with rank columns (`kingdom` … `species` or
#'   `Genus` / `Species`) and a `taxa` column matching `df$taxa`. Row names may
#'   be used instead of `taxa`.
#' @param top Integer. Most abundant tips to draw in the ggplot layout.
#'
#' @return A plot object ([ggplot2::ggplot], or the metacoder heat-tree grob).
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path, pattern = "m1[12]_", trim_char = "_")
#' df2heattree(df[df$clade == "S", ], top = 15)
#'
#' @export
#' @importFrom rlang .data
df2heattree <- function(df, tax = NULL, top = 20L) {
  if (!is.null(tax) && requireNamespace("metacoder", quietly = TRUE)) {
    return(archi_heattree_metacoder(df, tax))
  }
  if (is.null(df)) stop("df2heattree needs a tidy table or metacoder ranks", call. = FALSE)
  df <- df_tidy_drop_unclassified(df)
  means <- dplyr::summarise(df, m = mean(.data$amount), .by = "taxa")
  means <- means[order(-means$m), , drop = FALSE]
  means <- utils::head(means, min(as.integer(top), nrow(means)))
  tree <- archi_label_tree(means$taxa)
  lay <- archi_phylo_layout(tree)
  nodes <- lay$nodes
  nodes$m <- means$m[match(nodes$label, means$taxa)]
  nodes$m[is.na(nodes$m)] <- stats::median(means$m)
  nodes$label[is.na(nodes$label)] <- ""
  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = lay$edges,
      ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
      colour = "grey60", linewidth = 0.3
    ) +
    ggplot2::geom_point(
      data = nodes[nzchar(nodes$label), , drop = FALSE],
      ggplot2::aes(x = .data$x, y = .data$y, size = .data$m, colour = .data$m)
    ) +
    ggplot2::geom_text(
      data = nodes[nzchar(nodes$label), , drop = FALSE],
      ggplot2::aes(x = .data$x + 0.08, y = .data$y, label = .data$label),
      hjust = 0, size = 2.5, fontface = "italic"
    ) +
    ggplot2::scale_colour_gradientn(
      colours = viridis::viridis(256), name = "mean amount"
    ) +
    ggplot2::scale_size_continuous(name = "mean amount", range = c(1.5, 6)) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")
}

#' metacoder::heat_tree from a rank table plus abundances
#' @keywords internal
archi_heattree_metacoder <- function(df, tax) {
  tax <- as.data.frame(tax, stringsAsFactors = FALSE)
  if (!"taxa" %in% names(tax)) {
    tax$taxa <- rownames(tax)
  }
  rank_cols <- intersect(
    c("kingdom", "phylum", "class", "order", "family", "genus", "species",
      "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
    names(tax)
  )
  if (length(rank_cols) < 2L) stop("tax needs at least two rank columns", call. = FALSE)
  input <- tax[, rank_cols, drop = FALSE]
  sample_cols <- character()
  if (!is.null(df)) {
    mat <- df_untidy(df, amount_from = "N", drop_unclassified = TRUE)
    idx <- match(tax$taxa, rownames(mat))
    abund <- matrix(
      0, nrow = nrow(tax), ncol = ncol(mat),
      dimnames = list(NULL, colnames(mat))
    )
    matched <- !is.na(idx)
    abund[matched, ] <- mat[idx[matched], , drop = FALSE]
    input <- cbind(input, as.data.frame(abund, check.names = FALSE))
    sample_cols <- colnames(mat)
  }
  obj <- metacoder::parse_tax_data(
    input, class_cols = rank_cols, named_by_rank = TRUE
  )
  if (length(sample_cols)) {
    obj$data$taxon_counts <- metacoder::calc_taxon_abund(
      obj, data = "tax_data", cols = sample_cols
    )
    obj$data$taxon_counts$total <- rowSums(
      obj$data$taxon_counts[, setdiff(names(obj$data$taxon_counts), "taxon_id"),
                            drop = FALSE]
    )
    return(rlang::inject(metacoder::heat_tree(
      obj,
      node_label = !!quote(taxon_names),
      node_size = !!quote(total),
      node_color = !!quote(total),
      node_color_axis_label = "Total reads",
      node_size_axis_label = "Total reads",
      layout = "davidson-harel",
      initial_layout = "reingold-tilford"
    )))
  }
  # heat_tree evaluates these names inside the Taxmap object. Without an
  # abundance table, n_obs is the number of input taxa under each node.
  rlang::inject(metacoder::heat_tree(
    obj,
    node_label = !!quote(taxon_names),
    node_size = !!quote(n_obs),
    node_color = !!quote(n_obs),
    node_color_axis_label = "Abundance",
    node_size_axis_label = "Taxa",
    layout = "davidson-harel",
    initial_layout = "reingold-tilford"
  ))
}

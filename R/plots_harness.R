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


#' Stop when an optional plotting package is missing
#' @keywords internal
archi_optional <- function(pkg, what) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(what, " needs the optional package ", pkg, call. = FALSE)
  }
}

#' Differential abundance tree
#'
#' Selects the taxa with the largest absolute log2 fold change between two
#' groups. The default engine is a \pkg{ggtree} cladogram
#' (`branch.length = "none"`, circular unless `layout = "rectangular"`) with a
#' [ggtreeExtra::geom_fruit] heatmap of relative abundance at each tip. Tip
#' colour is the log2 fold change. `fruit = "bar"` restores the single
#' fold-change column. `engine = "metacoder"` colours a metacoder heat tree by
#' the same fold change and stops when \pkg{metacoder} is not installed.
#' `engine = "microbiota"` runs \pkg{MicrobiotaProcess} `mp_diff_analysis` and
#' draws the ggtree / ggtreeExtra biomarker tree from that vignette; it stops
#' when MicrobiotaProcess is not installed.
#'
#' @param df A tidy table from [get_counts()].
#' @param group Column index or name with at least two levels (for example the
#'   legend column `stage`).
#' @param contrast Character vector of length 2 naming the groups. The fold
#'   change is `log2(contrast[2] / contrast[1])`.
#' @param max_tips Integer. How many taxa to keep.
#' @param layout `"circular"` (default) or `"rectangular"`. Ignored when
#'   `engine` is `"metacoder"` or `"microbiota"`.
#' @param tree Optional [ape::phylo]. Tips must match `taxa` values. When
#'   `NULL`, a genus/species formula tree is built from the labels. Used only
#'   by the ggtree engine.
#' @param engine `"ggtree"`, `"metacoder"`, or `"microbiota"`.
#' @param fruit `"heatmap"` (default) or `"bar"`. Used only by the ggtree engine.
#' @param tax Optional rank table (`kingdom` … `species`, plus `taxa`). Used
#'   by the metacoder and microbiota engines. When `NULL`, genus and species
#'   are parsed from the taxon labels.
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
                        layout = c("circular", "rectangular"),
                        tree = NULL,
                        engine = c("ggtree", "metacoder", "microbiota"),
                        fruit = c("heatmap", "bar"),
                        tax = NULL) {
  layout <- match.arg(layout)
  engine <- match.arg(engine)
  fruit <- match.arg(fruit)
  if (identical(engine, "microbiota")) {
    return(archi_difftree_microbiota(df, group, contrast, tax, max_tips))
  }
  lfc <- archi_group_lfc(df, group, contrast = contrast)
  lfc$abs_lfc <- abs(lfc$log2_lfc)
  lfc <- lfc[order(-lfc$abs_lfc), , drop = FALSE]
  lfc <- utils::head(lfc, min(as.integer(max_tips), nrow(lfc)))
  if (nrow(lfc) < 2L) stop("Need at least two taxa for a differential tree", call. = FALSE)

  if (identical(engine, "metacoder")) {
    archi_optional("metacoder", "df2difftree(engine = \"metacoder\")")
    kept <- df[df$taxa %in% lfc$id, , drop = FALSE]
    return(archi_difftree_metacoder(kept, lfc, tax))
  }

  if (is.null(tree)) {
    tree <- archi_label_tree(lfc$id)
  } else {
    tree <- ape::keep.tip(tree, intersect(tree$tip.label, lfc$id))
  }
  keep <- intersect(lfc$id, tree$tip.label)
  lfc <- lfc[match(keep, lfc$id), , drop = FALSE]
  if (nrow(lfc) < 2L) stop("Tree and taxa share fewer than two tips", call. = FALSE)
  tree <- ape::keep.tip(tree, lfc$id)
  groups <- archi_sample_group(df, group, unique(as.character(df$sample)))
  archi_difftree_ggtree(tree, lfc, layout, df, groups, fruit)
}

#' ggtree differential tree
#'
#' Default fruit is a heatmap of relative abundance (samples in the two
#' groups, or the two group means when there are many samples). Tip colour is
#' log2 fold change. `fruit = "bar"` keeps a single fold-change column.
#' ggtreeExtra looks geoms up with `do.call` inside its own namespace, so the
#' layer is built from the imported `geom_text` name and then swapped.
#'
#' @keywords internal
archi_difftree_ggtree <- function(tree, lfc, layout, df, groups, fruit) {
  lim <- max(abs(lfc$log2_lfc), na.rm = TRUE)
  if (!is.finite(lim) || lim <= 0) lim <- 1
  is_circ <- identical(layout, "circular")
  tip_offset <- if (is_circ) 0.35 else 0.15
  fruit_offset <- if (is_circ) 0.08 else 0.05
  fruit_pwidth <- if (identical(fruit, "heatmap")) {
    if (is_circ) 0.55 else 0.45
  } else if (is_circ) {
    0.35
  } else {
    0.40
  }

  display <- as.character(lfc$id)
  tip_meta <- data.frame(
    label = display,
    display = display,
    is_italic = grepl(" ", display, fixed = TRUE),
    log2_lfc = as.numeric(lfc$log2_lfc),
    stringsAsFactors = FALSE
  )

  archi_prepare_ggtree()
  if (is_circ) {
    p <- ggtree::ggtree(tree, layout = "circular", open.angle = 10,
                        branch.length = "none")
  } else {
    p <- ggtree::ggtree(tree, layout = "rectangular", branch.length = "none")
  }
  p <- ggtree::`%<+%`(p, tip_meta)
  p <- p + ggtree::geom_tippoint(
    ggplot2::aes(color = log2_lfc), size = 1.6, show.legend = TRUE
  )
  if (any(tip_meta$is_italic)) {
    p <- p + ggtree::geom_tiplab(
      ggplot2::aes(label = display, subset = is_italic),
      size = 2.2, offset = tip_offset, align = TRUE, linesize = 0.1,
      fontface = "italic"
    )
  }
  if (any(!tip_meta$is_italic)) {
    p <- p + ggtree::geom_tiplab(
      ggplot2::aes(label = display, subset = !is_italic),
      size = 2.2, offset = tip_offset, align = TRUE, linesize = 0.1,
      fontface = "plain"
    )
  }

  if (identical(fruit, "bar")) {
    bars <- data.frame(
      id = display,
      lfc = as.numeric(lfc$log2_lfc),
      stringsAsFactors = FALSE
    )
    p <- p +
      archi_geom_fruit(
        data = bars,
        mapping = ggplot2::aes(y = id, x = lfc, fill = lfc),
        offset = fruit_offset,
        pwidth = fruit_pwidth
      ) +
      ggplot2::scale_fill_gradient2(
        low = "#1B9E77", mid = "gray80", high = "#D81B60",
        midpoint = 0, limits = c(-lim, lim), name = "log2 LFC"
      )
  } else {
    heat <- archi_difftree_heat(df, groups, lfc)
    p <- p +
      archi_geom_fruit(
        data = heat,
        mapping = ggplot2::aes(y = id, x = sample, fill = amount),
        offset = fruit_offset,
        pwidth = fruit_pwidth,
        geom = ggplot2::geom_tile,
        geomname = "geom_tile"
      ) +
      ggplot2::scale_fill_viridis_c(name = "Relative abundance", option = "cividis")
  }
  p <- p +
    ggplot2::scale_color_gradient2(
      low = "#1B9E77", mid = "gray80", high = "#D81B60",
      midpoint = 0, limits = c(-lim, lim), name = "log2 LFC"
    ) +
    ggplot2::theme(legend.position = "right")
  if (is_circ) {
    p <- p + ggplot2::expand_limits(x = c(0, 4))
  } else if (requireNamespace("ggtree", quietly = TRUE) &&
             exists("hexpand", envir = asNamespace("ggtree"), inherits = FALSE)) {
    p <- p + ggtree::hexpand(0.85)
  }
  p
}

#' Sample or group abundance matrix for the differential-tree heatmap
#' @keywords internal
archi_difftree_heat <- function(df, groups, lfc) {
  lev <- c(lfc$group_low[[1]], lfc$group_high[[1]])
  use <- names(groups)[groups %in% lev]
  use <- use[order(match(unname(groups[use]), lev), use)]
  sub <- df[as.character(df$taxa) %in% lfc$id & as.character(df$sample) %in% use, , drop = FALSE]
  if (!nrow(sub)) stop("No samples in the two contrast groups", call. = FALSE)
  if (length(use) > 12L) {
    sub$sample <- unname(groups[as.character(sub$sample)])
    use <- lev
  }
  heat <- dplyr::summarise(
    sub, amount = mean(.data$amount), .by = c("taxa", "sample")
  )
  heat$sample <- factor(as.character(heat$sample), levels = unique(as.character(use)))
  data.frame(
    id = as.character(heat$taxa),
    sample = heat$sample,
    amount = as.numeric(heat$amount),
    stringsAsFactors = FALSE
  )
}

#' ggplot2 4 renamed is.waive; released ggtree still calls it while drawing
#' @keywords internal
archi_prepare_ggtree <- function() {
  if (exists("is.waive", envir = asNamespace("ggplot2"), inherits = FALSE)) {
    return(invisible(NULL))
  }
  if (!exists("is.waive", mode = "function", inherits = TRUE)) {
    assign("is.waive", function(x) inherits(x, "waiver"), envir = .GlobalEnv)
  }
  invisible(NULL)
}

#' Fruit layer that ggtreeExtra can construct from inside another package
#'
#' `geom_text` is imported by ggtreeExtra, so its name check succeeds.
#' `geom_col`, `geom_tile` and `geom_boxplot` are not, and `do.call` of those
#' names inside that namespace fails. The layer is built under the imported
#' name and then swapped. `axis.params` and `grid.params` must be literal
#' `list()` calls: `geom_fruit` captures them with `enquo`.
#'
#' @keywords internal
archi_geom_fruit <- function(data = NULL, mapping, offset, pwidth,
                             geom = ggplot2::geom_col,
                             geomname = "geom_col",
                             position = NULL,
                             ...) {
  # Two calls, not do.call: geom_fruit captures axis.params with enquo.
  if (is.null(data)) {
    fruit <- ggtreeExtra::geom_fruit(
      geom = geom_text,
      mapping = mapping,
      offset = offset,
      pwidth = pwidth,
      orientation = "y",
      axis.params = list(
        axis = "x", text.size = 1.8, hjust = 1, vjust = 0.5, nbreak = 3
      ),
      grid.params = list()
    )
  } else {
    fruit <- ggtreeExtra::geom_fruit(
      data = data,
      geom = geom_text,
      mapping = mapping,
      offset = offset,
      pwidth = pwidth,
      orientation = "y",
      axis.params = list(
        axis = "x", text.size = 1.8, hjust = 1, vjust = 0.5, nbreak = 3
      ),
      grid.params = list()
    )
  }
  fruit$geom <- geom
  fruit$geomname <- geomname
  # ggtreeExtra forwards orientation = "y". geom_tile and geom_star warn on it.
  if (geomname %in% c("geom_tile", "geom_star")) {
    fruit$geom <- function(...) {
      args <- list(...)
      args$orientation <- NULL
      do.call(geom, args)
    }
  }
  if (!is.null(position)) {
    fruit$params$position <- position
  } else if (identical(geomname, "geom_col")) {
    fruit$params$position <- ggtreeExtra::position_stackx()
  }
  extra <- list(...)
  if (length(extra)) fruit$params <- utils::modifyList(fruit$params, extra)
  fruit
}

#' Horizontal boxplot position that shifts xmin/xmax, not a missing x column
#' @keywords internal
archi_position_boxx <- function() {
  ggplot2::ggproto(
    NULL,
    utils::getFromNamespace("PositionIdentityx", "ggtreeExtra"),
    compute_layer = function(self, data, params, layout) {
      if (!is.na(params$hexpand)) {
        for (col in c(
          "x", "xmin", "xmax", "xlower", "xmiddle", "xupper",
          "xmin_final", "xmax_final"
        )) {
          if (col %in% names(data)) data[[col]] <- data[[col]] + params$hexpand
        }
      }
      data
    }
  )
}

#' MicrobiotaProcess biomarker tree (mp_diff_analysis + ggtree)
#'
#' The layout follows the MicrobiotaProcess biomarker vignette, which is drawn
#' with ggtree and ggtreeExtra: radial taxonomy tree, phylum highlight,
#' per-sample abundance stars, LDA bars and FDR points.
#'
#' @keywords internal
archi_difftree_microbiota <- function(df, group, contrast, tax, max_tips) {
  archi_optional("MicrobiotaProcess", "df2difftree(engine = \"microbiota\")")
  archi_optional("ggstar", "df2difftree(engine = \"microbiota\")")
  archi_optional("phyloseq", "df2difftree(engine = \"microbiota\")")
  mpse <- archi_mpse_from_df(df, group, contrast, tax, max_tips)
  group_name <- "archi_group"
  prepared <- archi_mpse_abundance(mpse)
  mpse <- archi_mp_diff(prepared$mpse, prepared$relative, group_name)
  tree <- MicrobiotaProcess::mp_extract_tree(mpse, type = "taxatree")
  archi_prepare_ggtree()
  archi_plot_mp_diff(tree, group_name)
}

#' Counts, taxonomy and a two-or-more group column as an MPSE
#' @keywords internal
archi_mpse_from_df <- function(df, group, contrast, tax, max_tips) {
  df <- df_tidy_drop_unclassified(df)
  groups <- archi_sample_group(df, group, unique(as.character(df$sample)))
  if (!is.null(contrast)) {
    if (length(contrast) != 2L || !all(contrast %in% groups)) {
      stop("contrast must be two group levels present in the table", call. = FALSE)
    }
    groups <- groups[groups %in% contrast]
  }
  lev <- unique(as.character(groups))
  if (length(lev) < 2L) {
    stop("df2difftree(engine = \"microbiota\") needs at least two groups", call. = FALSE)
  }
  df <- df[as.character(df$sample) %in% names(groups), , drop = FALSE]
  totals <- tapply(as.numeric(df$N), as.character(df$taxa), sum)
  totals <- sort(totals, decreasing = TRUE)
  cap <- max(40L, as.integer(max_tips))
  keep_taxa <- names(totals)[seq_len(min(cap, length(totals)))]
  df <- df[as.character(df$taxa) %in% keep_taxa, , drop = FALSE]
  mat <- df_untidy(df, amount_from = "N", drop_unclassified = TRUE)
  mat <- round(mat)
  mat[mat < 0 | !is.finite(mat)] <- 0
  storage.mode(mat) <- "integer"
  mat <- mat[, intersect(names(groups), colnames(mat)), drop = FALSE]
  tax_mat <- archi_mpse_taxonomy(rownames(mat), tax)
  sam <- data.frame(
    archi_group = factor(unname(groups[colnames(mat)]), levels = lev),
    row.names = colnames(mat),
    stringsAsFactors = FALSE
  )
  ps <- phyloseq::phyloseq(
    phyloseq::otu_table(mat, taxa_are_rows = TRUE),
    phyloseq::tax_table(as.matrix(tax_mat)),
    phyloseq::sample_data(sam)
  )
  MicrobiotaProcess::as.MPSE(ps)
}

#' Capitalised rank table indexed by taxon label
#' @keywords internal
archi_mpse_taxonomy <- function(taxa, tax) {
  ranks <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  out <- as.data.frame(
    matrix(NA_character_, nrow = length(taxa), ncol = length(ranks),
           dimnames = list(taxa, ranks)),
    stringsAsFactors = FALSE
  )
  if (is.null(tax)) {
    genus <- ifelse(grepl(" ", taxa), sub(" .*", "", taxa), taxa)
    out$Kingdom <- "Bacteria"
    out$Genus <- genus
  } else {
    tax <- as.data.frame(tax, stringsAsFactors = FALSE)
    if (!"taxa" %in% names(tax)) tax$taxa <- rownames(tax)
    names(tax) <- tolower(names(tax))
    tax <- tax[match(taxa, as.character(tax$taxa)), , drop = FALSE]
    for (rk in tolower(ranks)) {
      if (!rk %in% names(tax)) next
      out[[paste0(toupper(substr(rk, 1, 1)), substr(rk, 2, nchar(rk)))]] <- as.character(tax[[rk]])
    }
    if (all(is.na(out$Genus))) {
      out$Genus <- ifelse(grepl(" ", taxa), sub(" .*", "", taxa), taxa)
    }
    if (all(is.na(out$Kingdom))) out$Kingdom <- "Bacteria"
  }
  # convert_to_treedata uses the rank strings as node ids, then adds the row
  # name as the tip. A tip that repeats a rank name makes an invalid phylo.
  # Rank prefixes keep those ids apart. Empty cells stay NA so fillNAtax can
  # fill them without copying the parent name.
  for (rk in ranks) {
    x <- as.character(out[[rk]])
    ok <- !is.na(x) & nzchar(x)
    x[ok] <- paste0(substr(tolower(rk), 1, 1), "__", x[ok])
    out[[rk]] <- x
  }
  as.matrix(out)
}

#' mp_diff_analysis, relaxed once if the first pass finds no biomarkers
#' @keywords internal
archi_mp_diff <- function(mpse, relative, group_name) {
  # A failed edge check inside tidytree prints this even when the taxatree
  # that mp_extract_tree returns is a usable phylo.
  old <- options(check.tbl_tree.verbose = FALSE)
  on.exit(options(old), add = TRUE)
  run <- function(alpha, lda) {
    rlang::inject(MicrobiotaProcess::mp_diff_analysis(
      mpse,
      .abundance = !!rlang::sym(relative),
      .group = !!rlang::sym(group_name),
      first.test.alpha = alpha,
      filter.p = "pvalue",
      strict = FALSE,
      cl.test = FALSE,
      subcl.test = FALSE,
      cl.min = 2,
      subcl.min = 2,
      ldascore = lda,
      bootnums = 20
    ))
  }
  fit <- run(0.05, 2)
  tree <- tryCatch(
    MicrobiotaProcess::mp_extract_tree(fit, type = "taxatree"),
    error = function(e) NULL
  )
  has_lda <- !is.null(tree) && "LDAmean" %in% names(tibble::as_tibble(tree)) &&
    any(!is.na(tibble::as_tibble(tree)$LDAmean))
  if (!has_lda) {
    fit2 <- tryCatch(run(0.2, 1), error = function(e) NULL)
    if (!is.null(fit2)) fit <- fit2
  }
  fit
}

#' Rarefy when possible, then store a relative-abundance assay
#' @keywords internal
archi_mpse_abundance <- function(mpse) {
  rare <- tryCatch(
    MicrobiotaProcess::mp_rrarefy(mpse, action = "add"),
    error = function(e) NULL
  )
  if (!is.null(rare)) mpse <- rare
  assays <- names(SummarizedExperiment::assays(mpse))
  raw <- if ("RareAbundance" %in% assays) "RareAbundance" else "Abundance"
  mpse <- rlang::inject(MicrobiotaProcess::mp_cal_abundance(
    mpse, .abundance = !!rlang::sym(raw), action = "add"
  ))
  mpse <- rlang::inject(MicrobiotaProcess::mp_cal_abundance(
    mpse, .abundance = !!rlang::sym(raw), .group = archi_group, action = "add"
  ))
  assays <- names(SummarizedExperiment::assays(mpse))
  relative <- assays[grepl("^Rel", assays) & grepl("Sample", assays)]
  if (!length(relative)) relative <- assays[grepl("^Rel", assays)]
  if (!length(relative)) {
    stop("MicrobiotaProcess did not store a relative-abundance assay", call. = FALSE)
  }
  list(mpse = mpse, relative = relative[[1]])
}

#' ggtree drawing of an mp_diff_analysis taxatree
#' @keywords internal
archi_plot_mp_diff <- function(tree, group_name) {
  td <- tibble::as_tibble(tree)
  sign_col <- paste0("Sign_", group_name)
  if (!sign_col %in% names(td)) {
    sign_col <- grep("^Sign_", names(td), value = TRUE)
    sign_col <- if (length(sign_col)) sign_col[[1]] else NA_character_
  }
  nested <- grep("^RareAbundanceBySample$", names(td), value = TRUE)
  if (!length(nested)) nested <- grep("^RareAbundance", names(td), value = TRUE)[[1]]
  stars <- tidyr::unnest(tibble::as_tibble(td), tidyr::all_of(nested))
  rel_col <- grep("^RelRare", names(stars), value = TRUE)
  if (!length(rel_col)) rel_col <- grep("Rel", names(stars), value = TRUE)
  rel_col <- rel_col[[1]]
  grp_col <- if (group_name %in% names(stars)) {
    group_name
  } else {
    setdiff(names(stars), c(names(td), rel_col, "Sample", nested))[[1]]
  }
  stars$rel <- as.numeric(stars[[rel_col]])
  stars$grp <- as.character(stars[[grp_col]])
  if (!"Sample" %in% names(stars)) {
    sample_col <- setdiff(names(stars), c(names(td), rel_col, "rel", "grp", nested))
    sample_col <- sample_col[vapply(stars[sample_col], is.atomic, logical(1))]
    names(stars)[names(stars) == sample_col[[1]]] <- "Sample"
  }
  stars <- stars[, c("label", "Sample", "rel", "grp"), drop = FALSE]
  levels <- unique(stars$grp)
  levels <- levels[!is.na(levels) & nzchar(levels)]
  pal <- stats::setNames(
    rep_len(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"), length(levels)),
    levels
  )
  p <- ggtree::ggtree(tree, layout = "radial", size = 0.3, branch.length = "none")
  internal <- p$data[!p$data$isTip, , drop = FALSE]
  p <- p + ggtree::geom_point(data = internal, fill = "white", size = 1, shape = 21)
  phyla <- p$data[!is.na(p$data$nodeClass) & p$data$nodeClass == "Phylum", , drop = FALSE]
  if (nrow(phyla)) {
    phyla$phylum <- phyla$label
    phyla$label <- NULL
    p <- p + ggtree::geom_hilight(
      data = phyla,
      ggplot2::aes(node = node, fill = phylum)
    )
  }
  p <- p +
    ggnewscale::new_scale_fill() +
    archi_geom_fruit(
      data = stars,
      mapping = ggplot2::aes(
        y = label, x = Sample, size = rel, fill = grp, subset = rel > 0
      ),
      offset = 0.04,
      pwidth = 0.8,
      geom = ggstar::geom_star,
      geomname = "geom_star",
      starshape = 13,
      starstroke = 0.25
    ) +
    ggplot2::scale_size_continuous(name = "Relative abundance (%)", range = c(0.5, 3)) +
    ggplot2::scale_fill_manual(values = pal, name = group_name) +
    ggtree::geom_tiplab(size = 2, offset = 7.2)
  if ("LDAmean" %in% names(td) && !is.na(sign_col) && sign_col %in% names(td)) {
    p <- p +
      ggnewscale::new_scale_fill() +
      archi_geom_fruit(
        data = NULL,
        mapping = rlang::inject(ggplot2::aes(
          x = LDAmean,
          fill = !!rlang::sym(sign_col),
          subset = !is.na(LDAmean)
        )),
        offset = 0.3,
        pwidth = 0.5,
        geom = ggplot2::geom_col,
        geomname = "geom_col"
      ) +
      ggnewscale::new_scale("size") +
      ggtree::geom_point(
        data = p$data[!is.na(p$data[[sign_col]]), , drop = FALSE],
        mapping = rlang::inject(ggplot2::aes(
          size = -log10(pmax(fdr, .Machine$double.xmin)),
          fill = !!rlang::sym(sign_col)
        )),
        shape = 21
      ) +
      ggplot2::scale_size_continuous(range = c(1, 3), name = expression(-log[10]~FDR)) +
      ggplot2::scale_fill_manual(values = pal, name = "Biomarker")
  }
  p + ggplot2::theme(
    legend.key.height = ggplot2::unit(0.3, "cm"),
    legend.key.width = ggplot2::unit(0.3, "cm"),
    legend.spacing.y = ggplot2::unit(0.02, "cm"),
    legend.text = ggplot2::element_text(size = 7),
    legend.title = ggplot2::element_text(size = 9)
  )
}

#' metacoder heat tree coloured by log2 fold change
#' @keywords internal
archi_difftree_metacoder <- function(df, lfc, tax) {
  obj <- archi_taxmap_abundance(df, tax)
  ids <- as.character(metacoder::taxon_ids(obj))
  nms <- as.character(metacoder::taxon_names(obj))
  name_of <- stats::setNames(nms, ids)
  tc <- obj$data$taxon_counts
  matched <- lfc$log2_lfc[match(unname(name_of[as.character(tc$taxon_id)]), lfc$id)]
  tc$log2_lfc <- ifelse(is.na(matched), 0, matched)
  obj$data$taxon_counts <- tc
  rlang::inject(metacoder::heat_tree(
    obj,
    node_label = !!quote(taxon_names),
    node_size = !!quote(n_obs),
    node_color = !!quote(log2_lfc),
    node_size_axis_label = "Taxa",
    node_color_axis_label = "log2 fold change",
    node_color_range = c("#1B9E77", "gray", "#D81B60"),
    layout = "davidson-harel",
    initial_layout = "reingold-tilford"
  ))
}

#' Presence / absence UpSet plot across groups
#'
#' A taxon is present in a group when it has a positive count in any sample of
#' that group. The drawing is [ComplexUpset::upset], the same plot the harness
#' script writes. The function stops when \pkg{ComplexUpset} is not installed.
#'
#' @param df A tidy table from [get_counts()].
#' @param group Column index or name.
#' @param min_size Integer. Drop intersections smaller than this.
#'
#' @return The plot returned by [ComplexUpset::upset].
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
#' df <- get_counts(path, pattern = "decont_b", legend = legend, trim_char = "_")
#' if (requireNamespace("ComplexUpset", quietly = TRUE)) {
#'   df2upset(df[df$clade == "G", ], group = "stage")
#' }
#'
#' @export
df2upset <- function(df, group, min_size = 1L) {
  archi_optional("ComplexUpset", "df2upset")
  df <- df_tidy_drop_unclassified(df)
  samples <- unique(as.character(df$sample))
  g <- archi_sample_group(df, group, samples)
  df$group <- unname(g[as.character(df$sample)])
  sets <- unique(as.character(df$group))
  sets <- sets[!is.na(sets) & nzchar(sets)]
  if (length(sets) < 2L) stop("df2upset needs at least two groups", call. = FALSE)
  mat <- df_untidy(df, amount_from = "N", drop_unclassified = TRUE)
  present <- sapply(sets, function(lv) {
    samps <- intersect(names(g)[g == lv], colnames(mat))
    if (!length(samps)) return(rep(0L, nrow(mat)))
    as.integer(rowSums(mat[, samps, drop = FALSE] > 0) > 0)
  })
  colnames(present) <- sets
  rownames(present) <- rownames(mat)
  upset_data <- as.data.frame(present, stringsAsFactors = FALSE)
  upset_data <- upset_data[rowSums(upset_data[, sets, drop = FALSE]) > 0, , drop = FALSE]
  if (!nrow(upset_data)) stop("No taxa with presence in any group", call. = FALSE)

  set_colors <- archi_set_palette(length(sets))
  names(set_colors) <- sets
  queries <- lapply(sets, function(set_name) {
    ComplexUpset::upset_query(
      set = set_name,
      fill = set_colors[[set_name]],
      only_components = "overall_sizes"
    )
  })

  old_theme <- ggplot2::theme_get()
  on.exit(ggplot2::theme_set(old_theme), add = TRUE)
  ggplot2::theme_set(ggplot2::theme_minimal() + ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_text(),
    axis.title.y = ggplot2::element_text()
  ))

  size_label_aes <- ggplot2::aes(
    label = !!ComplexUpset::get_size_mode("exclusive_intersection")
  )
  ComplexUpset::upset(
    upset_data,
    intersect = sets,
    name = "Groups",
    width_ratio = 0.2,
    stripes = "white",
    min_size = as.integer(min_size),
    base_annotations = list(
      "Taxa in sets" = (
        ComplexUpset::intersection_size(
          bar_number_threshold = 2000,
          text_mapping = size_label_aes,
          text = list(check_overlap = TRUE, size = 3)
        ) +
          ggplot2::theme(panel.grid = ggplot2::element_blank())
      )
    ),
    set_sizes = (
      ComplexUpset::upset_set_size() +
        ggplot2::geom_text(
          ggplot2::aes(label = ggplot2::after_stat(count)),
          hjust = 1.1, stat = "count", size = 3
        ) +
        ggplot2::scale_y_reverse(n.breaks = 3) +
        ggplot2::ylab("Taxa sets") +
        ggplot2::theme(
          plot.margin = ggplot2::margin(5.5, 5.5, 5.5, (5.5 + 1.2 * 10) * 1.5, unit = "pt")
        )
    ),
    themes = ComplexUpset::upset_modify_themes(list(
      "intersections_matrix" = ggplot2::theme(
        axis.text.y = ggplot2::element_text(face = "italic"),
        axis.title.x = ggplot2::element_text(),
        axis.title.y = ggplot2::element_blank()
      ),
      "overall_sizes" = ggplot2::theme(
        plot.margin = ggplot2::margin(5.5, 5.5, 5.5, (5.5 + 12) * 1.5, unit = "pt")
      )
    )),
    sort_intersections_by = c("degree", "cardinality"),
    sort_intersections = "descending",
    sort_sets = FALSE,
    queries = queries
  )
}

#' Set1-like colours for UpSet set-size bars
#' @keywords internal
archi_set_palette <- function(n) {
  n <- as.integer(n)
  base <- c(
    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
    "#FFFF33", "#A65628", "#F781BF", "#999999"
  )
  if (n <= length(base)) return(base[seq_len(n)])
  grDevices::colorRampPalette(base)(n)
}

#' Taxonomic heat tree (metacoder only)
#'
#' Builds a metacoder taxmap and draws [metacoder::heat_tree]. Node size is
#' the number of taxa under the node (`n_obs`). Colour is mean relative
#' abundance. There is no ggplot fallback: the function stops when
#' \pkg{metacoder} is not installed. Empty ranks are dropped and their
#' children reattached, as [metacoder::parse_phyloseq] does, so the graph is
#' one tree. A `root` node is added only when the ranks would otherwise be a
#' forest (for example genus and species parsed from labels alone).
#'
#' @param df A tidy table from [get_counts()].
#' @param tax Optional data frame with rank columns (`kingdom` … `species` or
#'   `Genus` / `Species`) and a `taxa` column matching `df$taxa`. Row names may
#'   be used instead of `taxa`. When `NULL`, genus and species are parsed from
#'   the taxon labels.
#' @param top Integer. Most abundant taxa to keep.
#'
#' @return The plot returned by [metacoder::heat_tree].
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path, pattern = "m1[12]_", trim_char = "_")
#' if (requireNamespace("metacoder", quietly = TRUE)) {
#'   df2heattree(df[df$clade == "S", ], top = 15)
#' }
#'
#' @export
df2heattree <- function(df, tax = NULL, top = 20L) {
  archi_optional("metacoder", "df2heattree")
  if (is.null(df)) stop("df2heattree needs a tidy table", call. = FALSE)
  df <- df_tidy_drop_unclassified(df)
  means <- dplyr::summarise(df, m = mean(.data$amount), .by = "taxa")
  means <- means[order(-means$m), , drop = FALSE]
  means <- utils::head(means, min(as.integer(top), nrow(means)))
  if (nrow(means) < 2L) stop("df2heattree needs at least two taxa", call. = FALSE)
  df <- df[df$taxa %in% means$taxa, , drop = FALSE]
  obj <- archi_taxmap_abundance(df, tax)
  ylgnbu <- c(
    "#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4",
    "#1D91C0", "#225EA8", "#253494", "#081D58"
  )
  rlang::inject(metacoder::heat_tree(
    obj,
    node_label = !!quote(taxon_names),
    node_size = !!quote(n_obs),
    node_color = !!quote(total),
    node_color_range = ylgnbu,
    edge_color_range = ylgnbu,
    node_size_axis_label = "Taxa",
    node_color_axis_label = "Mean relative abundance",
    layout = "davidson-harel",
    initial_layout = "reingold-tilford"
  ))
}

#' Taxmap with per-taxon mean relative abundance
#'
#' Relative abundance is the tidy `amount` column (mean across samples stored
#' as `total`, matching the harness heat tree). `leaf` is the summed relative
#' abundance. The taxonomy graph is one tree: empty ranks are removed the way
#' [metacoder::parse_phyloseq] drops taxa named `"NA"`, and a single `root`
#' node is added when the remaining ranks would otherwise be a forest.
#'
#' @keywords internal
archi_taxmap_abundance <- function(df, tax) {
  tax <- archi_heattree_taxonomy(df, tax)
  tax <- tax[tax$taxa %in% unique(as.character(df$taxa)), , drop = FALSE]
  if (nrow(tax) < 2L) stop("Taxonomy and the table share fewer than two taxa", call. = FALSE)
  mat <- df_untidy(df, amount_from = "amount", drop_unclassified = TRUE)
  archi_taxmap_from_matrix(tax, mat)
}

#' Build a connected Taxmap from a rank table and a taxa-by-sample matrix
#' @keywords internal
archi_taxmap_from_matrix <- function(tax, mat) {
  prep <- archi_connect_ranks(tax)
  tax <- prep$tax
  rank_cols <- prep$rank_cols
  idx <- match(as.character(tax$taxa), rownames(mat))
  abund <- matrix(
    0, nrow = nrow(tax), ncol = ncol(mat),
    dimnames = list(NULL, colnames(mat))
  )
  matched <- !is.na(idx)
  abund[matched, ] <- mat[idx[matched], , drop = FALSE]
  input <- cbind(tax[, rank_cols, drop = FALSE], as.data.frame(abund, check.names = FALSE))
  obj <- metacoder::parse_tax_data(
    input, class_cols = rank_cols, named_by_rank = TRUE
  )
  # parse_phyloseq does this: drop "NA" nodes and reattach their children,
  # so a missing rank does not split the tree into a forest.
  obj <- archi_taxmap_drop_na_names(obj)
  archi_assert_taxmap_tree(obj)
  sample_cols <- colnames(mat)
  obj$data$taxon_counts <- metacoder::calc_taxon_abund(
    obj, data = "tax_data", cols = sample_cols
  )
  num_cols <- intersect(sample_cols, names(obj$data$taxon_counts))
  mat_tc <- as.matrix(obj$data$taxon_counts[, num_cols, drop = FALSE])
  obj$data$taxon_counts$total <- rowMeans(mat_tc)
  obj$data$taxon_counts$leaf <- rowSums(mat_tc)
  obj
}

#' One shared root, and no all-empty rank columns
#' @keywords internal
archi_connect_ranks <- function(tax) {
  preferred <- c(
    "kingdom", "phylum", "class", "order", "family", "genus", "species"
  )
  rank_cols <- names(tax)[tolower(names(tax)) %in% preferred]
  rank_cols <- rank_cols[order(match(tolower(rank_cols), preferred))]
  if (!length(rank_cols)) stop("tax needs at least one rank column", call. = FALSE)
  for (col in rank_cols) {
    x <- as.character(tax[[col]])
    x[is.na(x) | !nzchar(x) | x == "NA"] <- NA_character_
    tax[[col]] <- x
  }
  nonempty <- vapply(rank_cols, function(col) any(!is.na(tax[[col]])), logical(1))
  rank_cols <- rank_cols[nonempty]
  if (!length(rank_cols)) stop("tax has no rank names", call. = FALSE)
  first <- tax[[rank_cols[[1]]]]
  if (length(unique(stats::na.omit(first))) != 1L || anyNA(first)) {
    tax$root <- "root"
    rank_cols <- c("root", rank_cols)
  }
  list(tax = tax, rank_cols = rank_cols)
}

#' Drop taxa named NA and reattach their children, as parse_phyloseq does
#' @keywords internal
archi_taxmap_drop_na_names <- function(obj) {
  nms <- obj$taxon_names()
  keep <- !is.na(nms) & nzchar(nms) & nms != "NA"
  if (all(keep)) return(obj)
  obj$filter_taxa(keep)
}

#' The taxonomy graph is one tree: a single root and one parent per node
#' @keywords internal
archi_assert_taxmap_tree <- function(obj) {
  ids <- as.character(obj$taxon_ids())
  nms <- obj$taxon_names()
  if (length(nms) != length(ids) || any(is.na(nms) | !nzchar(nms) | nms == "NA")) {
    stop("Taxonomy graph still contains NA nodes", call. = FALSE)
  }
  # roots() is an index into taxon_ids(), not a taxon id. edge_list stores
  # the root as from = NA, so that row is not a second parent.
  root_idx <- obj$roots()
  if (length(root_idx) != 1L || anyNA(root_idx) || root_idx < 1L || root_idx > length(ids)) {
    stop(
      "Taxonomy graph has ", length(root_idx),
      " roots; a heat tree needs one connected tree",
      call. = FALSE
    )
  }
  root_id <- ids[[root_idx]]
  el <- obj$edge_list
  from <- as.character(el$from)
  to <- as.character(el$to)
  real <- !is.na(el$from)
  if (any(!from[real] %in% ids) || any(!to %in% ids)) {
    stop("Taxonomy graph has an edge to a missing node", call. = FALSE)
  }
  children <- to[real]
  tips <- setdiff(ids, root_id)
  if (length(children) != length(tips) || any(duplicated(children)) ||
      !setequal(children, tips)) {
    stop("Taxonomy graph is not a tree: a node is missing a parent or has two", call. = FALSE)
  }
  invisible(obj)
}

#' Rank table for a heat tree, parsed from labels when `tax` is missing
#' @keywords internal
archi_heattree_taxonomy <- function(df, tax) {
  if (is.null(tax)) {
    labels <- unique(as.character(df$taxa))
    labels <- labels[!is.na(labels) & nzchar(labels)]
    genus <- ifelse(grepl(" ", labels), sub(" .*", "", labels), labels)
    return(data.frame(
      taxa = labels, genus = genus, species = labels,
      stringsAsFactors = FALSE
    ))
  }
  tax <- as.data.frame(tax, stringsAsFactors = FALSE)
  if (!"taxa" %in% names(tax)) tax$taxa <- rownames(tax)
  tax$taxa <- as.character(tax$taxa)
  tax
}

#' Convert a phyloseq object to a metacoder Taxmap
#'
#' Same conversion as the harness `phyloseq2metacoder` skill. A real
#' `phyloseq` object goes through [metacoder::parse_phyloseq] (that function
#' only sees `ranks_ref` after \pkg{metacoder} is attached). An
#' `archi_phyloseq` list, and any result that is still a forest, is parsed
#' from the rank table. Empty ranks are removed and children are reattached,
#' and a `root` node is added when the ranks do not already share one parent.
#' The returned graph is one tree.
#'
#' @param physeq A `phyloseq` object or an `archi_phyloseq` list.
#' @param to_relative Logical. Divide each sample by its total before parsing,
#'   as the harness does.
#'
#' @return A `Taxmap`.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
#' if (requireNamespace("metacoder", quietly = TRUE)) {
#'   ps <- kraken_to_phyloseq(path, pattern = "m1[12]_", legend = legend,
#'                            trim_char = "_", rank = "G")
#'   phyloseq_to_metacoder(ps)
#' }
#'
#' @export
phyloseq_to_metacoder <- function(physeq, to_relative = TRUE) {
  archi_optional("metacoder", "phyloseq_to_metacoder")
  if (inherits(physeq, "phyloseq")) {
    obj <- archi_parse_phyloseq(physeq, to_relative = to_relative)
    if (!is.null(obj) && length(obj$roots()) == 1L &&
        !any(obj$taxon_names() == "NA", na.rm = TRUE)) {
      return(obj)
    }
  }
  parts <- .unpack_phyloseq(physeq)
  if (is.null(parts$tax)) stop("phyloseq_to_metacoder needs a taxonomy table", call. = FALSE)
  tax <- as.data.frame(parts$tax, stringsAsFactors = FALSE)
  if (!"taxa" %in% names(tax)) tax$taxa <- rownames(tax)
  mat <- as.matrix(parts$otu)
  if (isTRUE(to_relative)) {
    totals <- colSums(mat)
    totals[!is.finite(totals) | totals <= 0] <- NA_real_
    mat <- sweep(mat, 2, totals, "/")
    mat[!is.finite(mat)] <- 0
  }
  archi_taxmap_from_matrix(tax, mat)
}

#' parse_phyloseq, after attaching metacoder so ranks_ref is visible
#' @keywords internal
archi_parse_phyloseq <- function(ps, to_relative = TRUE) {
  if (!requireNamespace("phyloseq", quietly = TRUE)) return(NULL)
  if (isTRUE(to_relative)) {
    ps <- phyloseq::transform_sample_counts(ps, function(x) {
      s <- sum(x)
      if (!is.finite(s) || s <= 0) x else x / s
    })
  }
  # ranks_ref is created on the search path by metacoder's attach hook.
  # parse_phyloseq looks it up from the metacoder namespace and misses it
  # until the package is attached.
  if (!"package:metacoder" %in% search()) {
    # Attach so ranks_ref is on the search path. library() is a check warning
    # for a suggested package; attachNamespace runs the same hook.
    attachNamespace(loadNamespace("metacoder"))
  }
  tryCatch(metacoder::parse_phyloseq(ps), error = function(e) NULL)
}

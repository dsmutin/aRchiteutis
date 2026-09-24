# Preliminary MultiQC-style report. Plot functions stay the df2* family;
# this wrapper only chooses them, writes captions, and records dropped samples.

archi_report_sources <- function() {
  c("auto", "kraken", "kaiju", "qza", "abundance")
}

archi_prepare_legend <- function(legend, target, trim_char) {
  if (is.null(legend) || !nzchar(legend)) {
    stop("archi_report() needs a QIIME 2-style legend", call. = FALSE)
  }
  meta <- archi_read_sample_metadata(legend)
  if (!isFALSE(trim_char)) {
    rownames(meta) <- vapply(
      strsplit(rownames(meta), trim_char, fixed = TRUE),
      function(z) z[[1]], character(1)
    )
  }
  if (!target %in% names(meta)) {
    stop(
      "Legend is missing the target column `", target, "`. Columns: ",
      paste(names(meta), collapse = ", "),
      call. = FALSE
    )
  }
  meta$target <- as.character(meta[[target]])
  tmp <- tempfile(fileext = ".csv")
  out <- cbind(data.frame("sample-id" = rownames(meta), check.names = FALSE), meta)
  utils::write.csv(out, tmp, row.names = FALSE)
  list(meta = meta, csv = tmp)
}

archi_report_load <- function(path, source, pattern, rank, legend_csv, trim_char,
                              counts, xml) {
  source <- match.arg(source, archi_report_sources())
  if (identical(source, "auto")) {
    bn <- if (dir.exists(path)) list.files(path, pattern = pattern) else basename(path)
    source <- if (any(grepl("\\.qza$", bn, ignore.case = TRUE))) {
      "qza"
    } else if (any(grepl("kaiju", bn, ignore.case = TRUE))) {
      "kaiju"
    } else if (!is.null(counts)) {
      "abundance"
    } else {
      "kraken"
    }
  }
  if (identical(source, "qza")) {
    return(qza_to_phyloseq(path, metadata = legend_csv))
  }
  if (identical(source, "kaiju")) {
    return(kaiju_to_phyloseq(path, pattern = pattern, legend = legend_csv, trim_char = trim_char))
  }
  if (identical(source, "abundance")) {
    if (is.null(counts)) stop("source = \"abundance\" needs `counts`", call. = FALSE)
    return(abundance_taxid_to_phyloseq(counts, metadata = legend_csv, xml = xml, trim_char = FALSE))
  }
  kraken_to_phyloseq(path, pattern = pattern, rank = rank, legend = legend_csv, trim_char = trim_char)
}

#' Flag samples that fail a read-count floor or a rarefaction curve
#'
#' Read depth uses `profile_reads` from the importer when available; otherwise
#' it is the sum of counts at the imported rank. A sample under `min_reads` is
#' dropped. Remaining samples are rarefied on all taxa at `curve_fraction` of
#' their rank-table depth. If observed richness still rises by more than
#' `curve_gain` between that point and full depth, the curve has not levelled
#' off and the sample is dropped.
#'
#' @param df Tidy aRchiteutis table.
#' @param min_reads Minimum per-sample count sum. The harness floor is 1000.
#' @param curve_gain Maximum allowed terminal relative gain in observed richness.
#' @param curve_fraction Fraction of rank-table reads used for the terminal
#'   rarefaction point.
#' @param curve_reps Rarefaction replicates used to estimate terminal richness.
#' @param seed RNG seed.
#' @return A data frame with columns `sample`, `reads`, `reason`, `detail`.
#'   Kept samples are not listed.
#' @keywords internal
archi_flag_samples <- function(df, min_reads = 1000, curve_gain = 0.15,
                               curve_fraction = 0.8, curve_reps = 5L,
                               seed = 123L) {
  count_totals <- tapply(as.numeric(df$N), as.character(df$sample), sum)
  totals <- count_totals
  if ("profile_reads" %in% names(df)) {
    profile <- tapply(as.numeric(df$profile_reads), as.character(df$sample), function(x) {
      x <- x[is.finite(x)]
      if (length(x)) x[[1]] else NA_real_
    })
    use <- is.finite(profile) & profile > 0
    totals[names(profile)[use]] <- profile[use]
  }
  totals[is.na(totals)] <- 0
  rows <- list()
  k <- 0L
  low <- names(totals)[totals < min_reads]
  for (s in low) {
    k <- k + 1L
    rows[[k]] <- data.frame(
      sample = s, reads = unname(totals[[s]]), reason = "read_count",
      detail = paste0("sum of counts ", totals[[s]], " < ", min_reads),
      stringsAsFactors = FALSE
    )
  }
  kept <- setdiff(names(totals), low)
  if (length(kept) && is.finite(curve_gain)) {
    sub <- df[as.character(df$sample) %in% kept, , drop = FALSE]
    mat <- df_untidy(sub, amount_from = "N", drop_unclassified = TRUE)
    curve_fraction <- max(0.1, min(0.99, as.numeric(curve_fraction)))
    curve_reps <- max(1L, as.integer(curve_reps))
    for (s in colnames(mat)) {
      counts <- mat[, s]
      n <- sum(counts)
      terminal_depth <- max(1L, as.integer(floor(n * curve_fraction)))
      obs_lo <- vapply(seq_len(curve_reps), function(rep) {
        set.seed(as.integer(seed) + match(s, colnames(mat)) * 1000L + rep)
        rare <- archi_rarefy_counts(counts, terminal_depth)
        if (is.null(rare)) NA_real_ else sum(rare > 0)
      }, numeric(1))
      obs_lo <- mean(obs_lo, na.rm = TRUE)
      obs_hi <- sum(counts > 0)
      if (!is.finite(obs_lo) || obs_lo <= 0) next
      gain <- (obs_hi - obs_lo) / max(obs_lo, 1)
      if (gain > curve_gain) {
        k <- k + 1L
        rows[[k]] <- data.frame(
          sample = s, reads = unname(totals[[s]]), reason = "diversity_curve",
          detail = paste0(
            "observed richness rose from ", signif(obs_lo, 5),
            " at depth ", terminal_depth,
            " to ", obs_hi, " at depth ", as.integer(n),
            " (relative gain ", signif(gain, 3), ")"
          ),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) {
    return(data.frame(sample = character(), reads = numeric(),
                      reason = character(), detail = character(),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

archi_report_catalog <- function(beta_method, order_samples, style) {
  bar_style <- if (identical(style, "raincloud")) "raincloud" else "box"
  list(
    composition = sprintf(
      "Stacked composition. Samples ordered by `%s`. Within-sample abundances rescaled to 0-1.",
      order_samples
    ),
    donut = "Mean composition across samples, drawn as a donut. Relative abundance.",
    barplot = sprintf(
      "Per-taxon distribution across samples. Geometry: `%s`.", bar_style
    ),
    alpha = sprintf(
      "Shannon and Simpson on counts. Split by target. Geometry: `%s`.", style
    ),
    beta = sprintf(
      "Sample distance heatmap. Method: `%s`. Samples clustered with ward.D2.",
      beta_method
    ),
    rarefaction = "Observed, Shannon and Simpson rarefaction. Thin line per sample, loess by target.",
    heattree = "Rank tree coloured by mean relative abundance. metacoder::heat_tree when that package and a taxonomy table are available; otherwise a formula tree.",
    upset = "Taxon presence intersections across target levels.",
    difftree = "Log2 fold change between the first two target levels on a taxonomy tree. ggtree fruit bars when ggtree and ggtreeExtra are installed."
  )
}

archi_report_taxonomy <- function(ps) {
  tax <- .unpack_phyloseq(ps)$tax
  if (is.null(tax)) return(NULL)
  tax <- as.data.frame(tax, stringsAsFactors = FALSE)
  names(tax) <- tolower(names(tax))
  ranks <- intersect(archi_rank_cols(), names(tax))
  if (length(ranks) < 2L) return(NULL)
  tax$taxa <- apply(tax[, ranks, drop = FALSE], 1, function(row) {
    row <- as.character(row)
    row <- row[!is.na(row) & nzchar(row)]
    if (length(row)) utils::tail(row, 1) else NA_character_
  })
  tax <- tax[!is.na(tax$taxa) & nzchar(tax$taxa), c("taxa", ranks), drop = FALSE]
  tax[!duplicated(tax$taxa), , drop = FALSE]
}

archi_report_draw <- function(id, df, tax, target_col, beta_method, order_samples,
                              style, top, rarefaction_depths, rarefaction_reps,
                              contrast = NULL) {
  trimmed <- df_get_top_taxa(df, top = top, drop_unclassified = TRUE)
  switch(id,
    composition = df2composition(trimmed, order_samples = order_samples),
    donut = df2donut(trimmed),
    barplot = df2barplot(trimmed, style = if (style == "raincloud") "raincloud" else "box"),
    alpha = df2alpha(df, split_by = target_col, style = style),
    beta = df2beta(df, clade = NULL, method = beta_method, add_legend = target_col),
    rarefaction = df2rarefaction(
      df, split_by = "target", depths = rarefaction_depths,
      n_reps = rarefaction_reps, top = top
    ),
    heattree = df2heattree(trimmed, tax = tax, top = top),
    upset = df2upset(df, group = "target"),
    difftree = df2difftree(
      df, group = "target", contrast = contrast, max_tips = min(20L, top)
    ),
    stop("Unknown plot id: ", id, call. = FALSE)
  )
}

archi_base64_file <- function(path) {
  size <- file.info(path)$size
  bytes <- as.integer(readBin(path, what = "raw", n = size))
  alphabet <- strsplit(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
    "", fixed = TRUE
  )[[1]]
  pad <- (3L - length(bytes) %% 3L) %% 3L
  if (pad) bytes <- c(bytes, rep(0L, pad))
  triples <- matrix(bytes, ncol = 3L, byrow = TRUE)
  encoded <- cbind(
    triples[, 1] %/% 4L,
    (triples[, 1] %% 4L) * 16L + triples[, 2] %/% 16L,
    (triples[, 2] %% 16L) * 4L + triples[, 3] %/% 64L,
    triples[, 3] %% 64L
  )
  chars <- alphabet[as.vector(t(encoded)) + 1L]
  if (pad) chars[(length(chars) - pad + 1L):length(chars)] <- "="
  paste(chars, collapse = "")
}

archi_mqc_image <- function(path, id, section, description, image) {
  data <- paste0(
    "<div class='mqc-custom-content-image'><img alt='",
    archi_html_escape(section),
    "' src='data:image/png;base64,", archi_base64_file(image), "' /></div>"
  )
  writeLines(c(
    paste0("id: \"", id, "\""),
    paste0("section_name: \"", section, "\""),
    paste0("description: \"", gsub("\"", "'", description), "\""),
    "plot_type: \"image\"",
    "data: |",
    paste0("  ", data),
    "pconfig:",
    paste0("  id: \"", id, "\""),
    paste0("  title: \"", section, "\"")
  ), path)
}

archi_mqc_table <- function(path, dropped) {
  data <- dropped
  if (!nrow(data)) {
    data <- data.frame(
      sample = "none", reads = 0, reason = "none",
      detail = "No samples were removed by the configured QC checks.",
      stringsAsFactors = FALSE
    )
  }
  header <- c(
    "# id: architeutis-dropped",
    "# section_name: Dropped samples",
    "# description: Samples removed before plotting because of read depth or a rarefaction curve that was still rising.",
    "# plot_type: table",
    "# pconfig:",
    "#   id: architeutis-dropped",
    "#   title: Dropped samples"
  )
  table_lines <- utils::capture.output(utils::write.table(
    data, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE
  ))
  writeLines(c(header, table_lines), path)
}

archi_html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

archi_write_report_html <- function(path, dropped, sections, disclaimer) {
  head <- c(
    "<!DOCTYPE html>",
    "<html><head><meta charset=\"utf-8\">",
    "<title>aRchiteutis preliminary report</title>",
    "<style>body{font-family:sans-serif;max-width:960px;margin:2rem auto;color:#222}",
    "img{max-width:100%} table{border-collapse:collapse} td,th{border:1px solid #ccc;padding:4px 8px}</style>",
    "</head><body>",
    "<h1>aRchiteutis preliminary report</h1>",
    "<h2>Dropped samples</h2>"
  )
  if (!nrow(dropped)) {
    table <- "<p>No samples were removed by the read-count floor or the rarefaction-curve check.</p>"
  } else {
    cells <- apply(dropped, 1, function(row) {
      paste0("<tr>", paste0("<td>", archi_html_escape(row), "</td>", collapse = ""), "</tr>")
    })
    table <- c(
      "<table><thead><tr><th>sample</th><th>reads</th><th>reason</th><th>detail</th></tr></thead><tbody>",
      cells, "</tbody></table>"
    )
  }
  body <- unlist(lapply(sections, function(s) {
    c(
      paste0("<h2>", archi_html_escape(s$title), "</h2>"),
      paste0("<p>", archi_html_escape(s$caption), "</p>"),
      if (!is.null(s$image)) paste0("<img src=\"", s$image, "\" alt=\"", archi_html_escape(s$title), "\">") else NULL,
      if (!is.null(s$note)) paste0("<p><em>", archi_html_escape(s$note), "</em></p>") else NULL
    )
  }))
  writeLines(c(head, table, body, paste0("<p><strong>", disclaimer, "</strong></p>"), "</body></html>"), path)
}

#' Preliminary MultiQC report for a taxonomic profile
#'
#' Reads every profile the chosen importer understands, builds a phyloseq
#' object with a taxonomy tree, and drops samples that fall below `min_reads`
#' or whose rarefaction curve is still rising. Dropped samples are written to
#' `dropped_samples.csv` and shown as a table. Each requested `df2*` plot is
#' saved with a one-line method caption as MultiQC custom content,
#' and the same panels are collected into `architeutis_report.html`.
#'
#' The HTML and the MultiQC section both end with the statement that this is
#' a preliminary report only, not a final analysis.
#'
#' @param path Directory of Kraken-family reports, Kaiju output, or QIIME 2
#'   artifacts. Ignored for `source = "abundance"` except as a label.
#' @param legend QIIME 2 manifest. Sample ids may be `sample-id`, `sample_id`,
#'   `SampleID`, `sample`, or the first column. `target` names the grouping
#'   column (`"target"`, or for the bundled legend `"stage"`).
#' @param outdir Directory to create. Existing files with the same names are
#'   overwritten.
#' @param pattern File filter for Kraken or Kaiju directories.
#' @param trim_char Passed to the importer so file names match legend ids.
#' @param rank Rank code for Kraken-family reports.
#' @param source One of `"auto"`, `"kraken"`, `"kaiju"`, `"qza"`, `"abundance"`.
#' @param target Column in `legend` copied to `target` for grouped plots.
#' @param min_reads Samples below this count sum are dropped (`read_count`).
#' @param curve_gain Samples whose observed richness rises by more than this
#'   fraction between `curve_fraction` depth and full depth are dropped
#'   (`diversity_curve`).
#' @param curve_fraction Terminal rarefaction fraction used by the curve check.
#' @param curve_reps Replicates used by the curve check.
#' @param plots Character vector of plot ids. Any of `composition`, `donut`,
#'   `barplot`, `alpha`, `beta`, `rarefaction`, `heattree`, `upset`, `difftree`.
#' @param beta_method Name passed to [df2beta()], including phyloseq distances
#'   and `"aitchison"` when the matching package is installed.
#' @param order_samples Sample order for [df2composition()].
#' @param style `"box"`, `"violin"` or `"raincloud"` for alpha and bar plots.
#' @param top Taxa kept before composition, rarefaction and heat-tree plots.
#' @param rarefaction_depths Depths for [df2rarefaction()]. `NULL` uses a short
#'   grid capped at the smallest kept library.
#' @param rarefaction_reps Replicates per rarefaction depth.
#' @param contrast Optional two target levels for `difftree`. Required when the
#'   target column has more than two levels.
#' @param strict Stop if a requested plot fails or does not return a ggplot.
#'   Set `FALSE` to keep the error as a note in the standalone HTML.
#' @param counts Abundance table for `source = "abundance"`.
#' @param xml NCBI taxonomy XML for the abundance importer. `NULL` fetches.
#' @return Invisibly, a list with `dropped`, `html`, `outdir` and `plots`.
#'
#' @examples
#' out <- tempfile("archi-report")
#' path <- system.file("extdata", package = "aRchiteutis")
#' legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
#' archi_report(
#'   path, legend, outdir = out, pattern = "m1[12]_", trim_char = "_",
#'   target = "stage", plots = c("composition", "rarefaction"),
#'   rarefaction_depths = c(500L, 2000L), rarefaction_reps = 1L, top = 8L
#' )
#'
#' @export
archi_report <- function(path, legend, outdir,
                         pattern = "",
                         trim_char = FALSE,
                         rank = "G",
                         source = c("auto", "kraken", "kaiju", "qza", "abundance"),
                         target = "target",
                         min_reads = 1000,
                         curve_gain = 0.15,
                         curve_fraction = 0.8,
                         curve_reps = 5L,
                         plots = c("composition", "donut", "alpha", "beta", "rarefaction"),
                         beta_method = "bray",
                         order_samples = c("fpc", "hclust", "abundance", "alpha", "none"),
                         style = c("box", "violin", "raincloud"),
                         top = 15L,
                         rarefaction_depths = NULL,
                         rarefaction_reps = 1L,
                         contrast = NULL,
                         strict = TRUE,
                         counts = NULL,
                         xml = NULL) {
  source <- match.arg(source)
  order_samples <- match.arg(order_samples)
  style <- match.arg(style)
  catalog <- archi_report_catalog(beta_method, order_samples, style)
  unknown <- setdiff(plots, names(catalog))
  if (length(unknown)) {
    stop("Unknown plots: ", paste(unknown, collapse = ", "),
         ". Known: ", paste(names(catalog), collapse = ", "), call. = FALSE)
  }
  prepared <- archi_prepare_legend(legend, target, trim_char)
  ps <- archi_report_load(path, source, pattern, rank, prepared$csv, trim_char, counts, xml)
  df <- from_phyloseq(ps)
  tax <- archi_report_taxonomy(ps)
  if (!"target" %in% names(df)) {
    stop("Imported table has no target column after the legend join", call. = FALSE)
  }
  dropped <- archi_flag_samples(
    df, min_reads = min_reads, curve_gain = curve_gain,
    curve_fraction = curve_fraction, curve_reps = curve_reps
  )
  kept_ids <- setdiff(unique(as.character(df$sample)), dropped$sample)
  df <- df[as.character(df$sample) %in% kept_ids, , drop = FALSE]

  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  mqc <- file.path(outdir, "multiqc")
  dir.create(mqc, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(dropped, file.path(outdir, "dropped_samples.csv"), row.names = FALSE)

  disclaimer <- "This is a preliminary report only, not a final analysis."
  archi_mqc_table(file.path(mqc, "architeutis-dropped_mqc.tsv"), dropped)
  writeLines(c(
    "id: \"architeutis-disclaimer\"",
    "section_name: \"Preliminary report\"",
    "description: \"Draft status\"",
    "plot_type: \"html\"",
    paste0("data: \"<p>", disclaimer, "</p>\"")
  ), file.path(mqc, "architeutis-disclaimer_mqc.yaml"))

  target_col <- match("target", names(df))
  sections <- list()
  written <- character()
  if (length(kept_ids) >= 1L) {
    for (id in plots) {
      drawn <- tryCatch(
        archi_report_draw(
          id, df, tax, target_col, beta_method, order_samples, style,
          top, rarefaction_depths, rarefaction_reps, contrast
        ),
        error = function(e) e
      )
      caption <- catalog[[id]]
      if (inherits(drawn, "error")) {
        if (isTRUE(strict)) {
          stop("Report plot `", id, "` failed: ", conditionMessage(drawn), call. = FALSE)
        }
        sections[[length(sections) + 1L]] <- list(
          title = id, caption = caption, image = NULL,
          note = conditionMessage(drawn)
        )
        next
      }
      if (!inherits(drawn, "ggplot")) {
        if (isTRUE(strict)) {
          stop("Report plot `", id, "` did not return a ggplot object", call. = FALSE)
        }
        sections[[length(sections) + 1L]] <- list(
          title = id, caption = caption, image = NULL,
          note = "The plot function did not return a ggplot object."
        )
        next
      }
      png <- file.path(mqc, paste0("archi-", id, ".png"))
      pdf <- file.path(outdir, paste0(id, ".pdf"))
      ggplot2::ggsave(png, drawn, width = 8, height = 5, dpi = 150)
      ggplot2::ggsave(pdf, drawn, width = 8, height = 5)
      archi_mqc_image(
        file.path(mqc, paste0("archi-", id, "_mqc.yaml")),
        paste0("archi-", id), id, caption, png
      )
      written <- c(written, png)
      sections[[length(sections) + 1L]] <- list(
        title = id, caption = caption,
        image = file.path("multiqc", basename(png)), note = NULL
      )
    }
  }
  html <- file.path(outdir, "architeutis_report.html")
  archi_write_report_html(html, dropped, sections, disclaimer)
  if (nzchar(Sys.which("multiqc"))) {
    try(system2("multiqc", c(mqc, "-o", file.path(outdir, "multiqc_report"), "-f"),
                stdout = FALSE, stderr = FALSE), silent = TRUE)
  }
  invisible(list(
    phyloseq = ps, dropped = dropped, html = html,
    outdir = outdir, plots = written
  ))
}

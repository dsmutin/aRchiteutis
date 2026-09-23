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
#' Read depth is the sum of counts in the tidy table (one rank, so clade reads
#' are not added twice). A sample under `min_reads` is dropped. Remaining
#' samples are rarefied, on the `top` taxa, at half their depth and at full
#' depth. If observed richness still rises by more than `curve_gain`, the
#' curve has not levelled off and the sample is dropped.
#'
#' @param df Tidy aRchiteutis table.
#' @param min_reads Minimum per-sample count sum. The harness floor is 1000.
#' @param curve_gain Maximum allowed relative gain in observed richness.
#' @param top Taxa used for the curve check.
#' @param seed RNG seed.
#' @return A data frame with columns `sample`, `reads`, `reason`, `detail`.
#'   Kept samples are not listed.
#' @keywords internal
archi_flag_samples <- function(df, min_reads = 1000, curve_gain = 0.5,
                               top = 40L, seed = 123L) {
  totals <- tapply(as.numeric(df$N), as.character(df$sample), sum)
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
    mat <- df_untidy(sub, amount_from = "N", top = top, drop_unclassified = TRUE)
    set.seed(as.integer(seed))
    for (s in colnames(mat)) {
      counts <- mat[, s]
      n <- sum(counts)
      half <- max(1L, as.integer(floor(n / 2)))
      lo <- archi_rarefy_counts(counts, half)
      hi <- archi_rarefy_counts(counts, as.integer(n))
      if (is.null(lo) || is.null(hi)) next
      obs_lo <- sum(lo > 0)
      obs_hi <- sum(hi > 0)
      gain <- (obs_hi - obs_lo) / max(obs_lo, 1)
      if (gain > curve_gain) {
        k <- k + 1L
        rows[[k]] <- data.frame(
          sample = s, reads = unname(totals[[s]]), reason = "diversity_curve",
          detail = paste0(
            "observed richness rose from ", obs_lo, " at depth ", half,
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
  list(
    composition = sprintf(
      "Stacked composition. Samples ordered by `%s`. Within-sample abundances rescaled to 0-1.",
      order_samples
    ),
    donut = "Mean composition across samples, drawn as a donut. Relative abundance.",
    barplot = sprintf(
      "Per-taxon distribution across samples. Geometry: `%s`.", style
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

archi_report_draw <- function(id, df, target_col, beta_method, order_samples,
                              style, top, rarefaction_depths, rarefaction_reps) {
  trimmed <- df_get_top_taxa(df, top = top, drop_unclassified = TRUE)
  switch(id,
    composition = df2composition(trimmed, order_samples = order_samples),
    donut = df2donut(trimmed),
    barplot = df2barplot(trimmed, style = if (style == "raincloud") "raincloud" else "box"),
    alpha = df2alpha(df, split_by = target_col, style = style),
    beta = df2beta(df, method = beta_method, add_legend = target_col),
    rarefaction = df2rarefaction(
      df, split_by = "target", depths = rarefaction_depths,
      n_reps = rarefaction_reps, top = top
    ),
    heattree = df2heattree(trimmed, top = top),
    upset = df2upset(df, group = "target"),
    difftree = df2difftree(df, group = "target", max_tips = min(20L, top)),
    stop("Unknown plot id: ", id, call. = FALSE)
  )
}

archi_mqc_yaml <- function(path, id, section, description) {
  writeLines(c(
    paste0("id: \"", id, "\""),
    paste0("section_name: \"", section, "\""),
    paste0("description: \"", gsub("\"", "'", description), "\""),
    "plot_type: \"image\"",
    "pconfig:",
    paste0("  id: \"", id, "\""),
    paste0("  title: \"", section, "\"")
  ), path)
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
#' saved with a one-line method caption as MultiQC custom-content PNG/YAML,
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
#'   fraction between half depth and full depth are dropped (`diversity_curve`).
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
                         curve_gain = 0.5,
                         plots = c("composition", "donut", "alpha", "beta", "rarefaction"),
                         beta_method = "bray",
                         order_samples = c("fpc", "hclust", "abundance", "alpha", "none"),
                         style = c("box", "violin", "raincloud"),
                         top = 15L,
                         rarefaction_depths = NULL,
                         rarefaction_reps = 1L,
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
  if (!"target" %in% names(df)) {
    stop("Imported table has no target column after the legend join", call. = FALSE)
  }
  dropped <- archi_flag_samples(df, min_reads = min_reads, curve_gain = curve_gain,
                                top = top)
  kept_ids <- setdiff(unique(as.character(df$sample)), dropped$sample)
  df <- df[as.character(df$sample) %in% kept_ids, , drop = FALSE]

  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  mqc <- file.path(outdir, "multiqc")
  dir.create(mqc, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(dropped, file.path(outdir, "dropped_samples.csv"), row.names = FALSE)

  disclaimer <- "This is a preliminary report only, not a final analysis."
  writeLines(c(
    "id: \"architeutis_dropped\"",
    "section_name: \"Dropped samples\"",
    "description: \"Samples removed before plotting because of read depth or a rarefaction curve that was still rising.\"",
    "plot_type: \"table\"",
    "pconfig:",
    "  id: \"architeutis_dropped\"",
    "  title: \"Dropped samples\""
  ), file.path(mqc, "architeutis-dropped_mqc.yaml"))
  writeLines(c(
    "id: \"architeutis_disclaimer\"",
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
        archi_report_draw(id, df, target_col, beta_method, order_samples, style,
                          top, rarefaction_depths, rarefaction_reps),
        error = function(e) e
      )
      caption <- catalog[[id]]
      if (inherits(drawn, "error")) {
        sections[[length(sections) + 1L]] <- list(
          title = id, caption = caption, image = NULL,
          note = conditionMessage(drawn)
        )
        next
      }
      if (!inherits(drawn, "ggplot")) {
        sections[[length(sections) + 1L]] <- list(
          title = id, caption = caption, image = NULL,
          note = "The plot function did not return a ggplot object."
        )
        next
      }
      png <- file.path(mqc, paste0("archi-", id, "_mqc.png"))
      pdf <- file.path(outdir, paste0(id, ".pdf"))
      ggplot2::ggsave(png, drawn, width = 8, height = 5, dpi = 150)
      ggplot2::ggsave(pdf, drawn, width = 8, height = 5)
      archi_mqc_yaml(file.path(mqc, paste0("archi-", id, "_mqc.yaml")),
                     paste0("archi_", id), id, caption)
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
  invisible(list(dropped = dropped, html = html, outdir = outdir, plots = written))
}

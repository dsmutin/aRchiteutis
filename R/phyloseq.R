# phyloseq <-> aRchiteutis long-df interoperability.
#
# The whole package is built around a single long (tidy) `tibble` produced by
# `get_counts()` whose first six columns have fixed positions:
#   1 taxa, 2 clade, 3 sample, 4 N, 5 amount, 6 amount_cl, 7+ legend/metadata.
# `ps2df()` turns a `phyloseq` object into exactly that structure so every
# `df2*` / `df_*` function can be driven straight from a `phyloseq` object, and
# `df2ps()` performs the reverse conversion.

# Kraken rank letters used across the package, coarsest -> finest.
.archi_clade_letters <- c("D", "P", "C", "O", "F", "G", "S")

# Map a (case-insensitive) taxonomic rank name to its Kraken clade letter.
.archi_rank_to_letter <- function(rank) {
  rank <- tolower(trimws(rank))
  map <- c(kingdom = "D", superkingdom = "D", domain = "D",
           phylum = "P", division = "P",
           class = "C", order = "O", family = "F",
           genus = "G", species = "S")
  unname(map[rank])
}

# Map a clade letter back to a canonical rank name for a tax_table column.
.archi_letter_to_rank <- function(letter) {
  map <- c(D = "Domain", P = "Phylum", C = "Class", O = "Order",
           F = "Family", G = "Genus", S = "Species")
  unname(map[letter])
}

# Error out early (with a helpful message) if phyloseq is not installed.
.archi_require_phyloseq <- function() {
  if (!requireNamespace("phyloseq", quietly = TRUE)) {
    stop("The 'phyloseq' package is required to work with phyloseq objects. ",
         "Install it with:\n",
         "  if (!requireNamespace('BiocManager')) install.packages('BiocManager')\n",
         "  BiocManager::install('phyloseq')",
         call. = FALSE)
  }
}

# Internal gateway used by every public entry point: transparently convert a
# `phyloseq` object to the long `df`, otherwise return the input untouched.
as_samovar_df <- function(x, ...) {
  if (methods::is(x, "phyloseq")) ps2df(x, ...) else x
}

# Internal helper for the matrix-consuming plots. A bare numeric matrix passes
# through unchanged (preserving legacy behaviour); a `phyloseq` object or a long
# `df` is reduced to a taxa-by-sample matrix with a sensible default
# `df_untidy()` call so those plots also accept high-level inputs.
as_samovar_matrix <- function(x, clade = "G", drop_unclassified = TRUE,
                              amount_from = "amount", top = FALSE,
                              scale = FALSE, ...) {
  if (methods::is(x, "phyloseq")) x <- ps2df(x, ...)
  if (is.data.frame(x)) {
    x <- df_untidy(x, clade = clade, amount_from = amount_from, top = top,
                   scale = scale, drop_unclassified = drop_unclassified)
  }
  x
}

#' Convert a phyloseq object into the aRchiteutis long count table
#'
#' Converts a [phyloseq::phyloseq] object into the long (tidy) `tibble` that the
#' rest of the package consumes, reproducing the hierarchical multi-clade
#' structure that `get_counts()` yields for Kraken2 reports. For **every**
#' taxonomic rank present in the `tax_table` the counts are aggregated (summed)
#' per sample and emitted as their own rows, tagged with the matching Kraken
#' clade letter, and a per-sample root row (`clade == "R"`) is added holding the
#' total classified reads.
#'
#' Ranks are matched case-insensitively and any subset of ranks is tolerated.
#' The rank -> clade-letter map is: Kingdom / Superkingdom / Domain -> `"D"`,
#' Phylum -> `"P"`, Class -> `"C"`, Order -> `"O"`, Family -> `"F"`,
#' Genus -> `"G"`, Species -> `"S"`. Empty / `NA` names at a given rank are
#' dropped for that rank. Because a standard `phyloseq` object records only
#' classified reads, the total and classified totals coincide, so `amount` and
#' `amount_cl` are equal.
#'
#' @param ps A [phyloseq::phyloseq] object. Must contain an `otu_table` and a
#'   `tax_table`; a `sample_data` slot is optional.
#' @param ... Ignored, for forward compatibility.
#'
#' @return A [tibble::tibble] whose first six columns are, in order, `taxa`
#'   (factor), `clade` (factor), `sample` (factor), `N` (numeric read count),
#'   `amount` (`N` / total reads per sample) and `amount_cl` (`N` / classified
#'   reads per sample). Any `sample_data` variables follow as columns 7+ and act
#'   as the legend.
#'
#' @seealso [df2ps()] for the reverse conversion and [get_counts()] for the
#'   native reader.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m1[124]_", trim_char = "_")
#' if (requireNamespace("phyloseq", quietly = TRUE)) {
#'   ps <- df2ps(df)          # build a phyloseq object from the example data
#'   df2 <- ps2df(ps)         # ... and convert it back
#'   head(df2)
#' }
#'
#' @export
ps2df <- function(ps, ...) {
  .archi_require_phyloseq()

  if (!methods::is(ps, "phyloseq")) {
    stop("`ps` must be a 'phyloseq' object.", call. = FALSE)
  }

  # --- counts: coerce to a taxa (rows) x samples (cols) matrix ---------------
  otu <- as(phyloseq::otu_table(ps), "matrix")
  if (!phyloseq::taxa_are_rows(ps)) otu <- t(otu)
  storage.mode(otu) <- "double"

  # --- lineage --------------------------------------------------------------
  tax <- as(phyloseq::tax_table(ps), "matrix")
  # Align the tax_table rows with the otu_table rows.
  tax <- tax[rownames(otu), , drop = FALSE]

  samples <- colnames(otu)
  totals <- colSums(otu)                       # classified == total per sample

  # For each rank column, find the clade letter it maps to (skip unknown ranks
  # and keep the finest column when several map to the same letter).
  col_letter <- vapply(colnames(tax), .archi_rank_to_letter, character(1))
  letter_col <- list()
  for (j in seq_along(col_letter)) {
    let <- col_letter[j]
    if (is.na(let)) next
    letter_col[[let]] <- j                      # last wins == finest column
  }

  parts <- list()
  for (letter in .archi_clade_letters) {
    j <- letter_col[[letter]]
    if (is.null(j)) next

    names_vec <- as.character(tax[, j])
    keep <- !is.na(names_vec) & nzchar(trimws(names_vec))
    if (!any(keep)) next

    agg <- rowsum(otu[keep, , drop = FALSE], group = names_vec[keep])
    nm <- rownames(agg)

    parts[[letter]] <- data.frame(
      taxa = rep(nm, times = length(samples)),
      clade = letter,
      sample = rep(samples, each = length(nm)),
      N = as.numeric(agg),
      stringsAsFactors = FALSE)
  }

  if (length(parts) == 0) {
    stop("The tax_table contains no recognisable taxonomic ranks.",
         call. = FALSE)
  }

  # Per-sample root row: total classified reads.
  root <- data.frame(
    taxa = "root", clade = "R", sample = samples,
    N = as.numeric(totals), stringsAsFactors = FALSE)

  res <- rbind(do.call(rbind, parts), root)
  res <- res[res$N > 0, , drop = FALSE]

  # amount / amount_cl: classified == total, so the two coincide.
  denom <- totals[res$sample]
  res$amount <- res$N / denom
  res$amount_cl <- res$amount

  # --- metadata / legend (columns 7+) ---------------------------------------
  sdata <- NULL
  if (!is.null(phyloseq::sample_data(ps, errorIfNULL = FALSE))) {
    sdata <- as(phyloseq::sample_data(ps), "data.frame")
  }

  res <- res[, c("taxa", "clade", "sample", "N", "amount", "amount_cl")]

  if (!is.null(sdata) && ncol(sdata) > 0) {
    sdata$sample <- rownames(sdata)
    res <- dplyr::left_join(res, sdata, by = "sample")
    # Restore the fixed 1-6 positions, legend afterwards.
    meta <- setdiff(names(res), c("taxa", "clade", "sample", "N",
                                  "amount", "amount_cl"))
    res <- res[, c("taxa", "clade", "sample", "N", "amount", "amount_cl", meta)]
  }

  res$taxa <- as.factor(res$taxa)
  res$clade <- as.factor(res$clade)
  res$sample <- factor(res$sample, levels = samples)

  tibble::as_tibble(res)
}

#' Convert the aRchiteutis long count table into a phyloseq object
#'
#' Builds a [phyloseq::phyloseq] object from the long `tibble` produced by
#' [get_counts()]. The finest clade present (or the one requested via `clade`)
#' defines the operational taxa; the parent lineage is recovered with
#' [df_get_parents()] and written to the `tax_table`, the per-sample counts (`N`)
#' become the `otu_table`, and any legend columns (7+) become the `sample_data`.
#'
#' A round-trip on genus-level counts (`ps2df(df2ps(df))`) preserves the genus
#' read counts and is a convenient correctness check.
#'
#' @param df A tidy `tibble` from [get_counts()] (with the full clade hierarchy).
#' @param clade Character or `FALSE`. Clade letter to use as the operational
#'   taxa (e.g. `"G"`). When `FALSE` (default) the finest clade present is used.
#' @param ... Ignored, for forward compatibility.
#'
#' @return A [phyloseq::phyloseq] object.
#'
#' @seealso [ps2df()] for the reverse conversion.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m1[124]_", trim_char = "_")
#' if (requireNamespace("phyloseq", quietly = TRUE)) {
#'   ps <- df2ps(df)
#'   ps
#' }
#'
#' @export
df2ps <- function(df, clade = FALSE, ...) {
  .archi_require_phyloseq()

  meta_names <- if (ncol(df) > 6) names(df)[7:ncol(df)] else character(0)

  parents <- suppressMessages(df_get_parents(df))

  # Choose the operational clade (finest present unless requested).
  if (isFALSE(clade)) {
    present <- .archi_clade_letters[.archi_clade_letters %in%
                                      as.character(parents$clade)]
    if (length(present) == 0) {
      stop("`df` contains none of the canonical clades D/P/C/O/F/G/S.",
           call. = FALSE)
    }
    clade <- present[length(present)]
  }

  sub <- parents[as.character(parents$clade) == clade, , drop = FALSE]
  if (nrow(sub) == 0) {
    stop("No rows found for clade '", clade, "' in `df`.", call. = FALSE)
  }

  # Lineage columns above the operational clade (added by df_get_parents()).
  higher <- .archi_clade_letters[seq_len(match(clade, .archi_clade_letters) - 1)]
  higher <- intersect(higher, names(sub))

  # --- otu_table: taxa (rows) x samples (cols) ------------------------------
  samples <- levels(factor(df$sample))
  counts <- dplyr::summarise(sub, N = sum(N), .by = c("taxa", "sample"))
  wide <- tidyr::pivot_wider(counts, names_from = "sample", values_from = "N",
                             id_cols = "taxa", values_fill = 0)
  taxa_names <- as.character(wide$taxa)
  otu <- as.matrix(wide[, -1, drop = FALSE])
  rownames(otu) <- taxa_names
  # Make sure every sample is represented, in a stable order.
  missing <- setdiff(samples, colnames(otu))
  if (length(missing) > 0) {
    add <- matrix(0, nrow = nrow(otu), ncol = length(missing),
                  dimnames = list(rownames(otu), missing))
    otu <- cbind(otu, add)
  }
  otu <- otu[, samples, drop = FALSE]

  # --- tax_table: lineage per operational taxon -----------------------------
  lineage <- as.data.frame(sub[!duplicated(as.character(sub$taxa)), ,
                               drop = FALSE])
  rownames(lineage) <- as.character(lineage$taxa)
  lineage <- lineage[taxa_names, , drop = FALSE]

  tax_cols <- c(higher, clade)
  tax <- matrix(NA_character_, nrow = length(taxa_names),
                ncol = length(tax_cols),
                dimnames = list(taxa_names,
                                vapply(tax_cols, .archi_letter_to_rank,
                                       character(1))))
  for (h in higher) tax[, .archi_letter_to_rank(h)] <- as.character(lineage[[h]])
  tax[, .archi_letter_to_rank(clade)] <- taxa_names

  ps_parts <- list(
    phyloseq::otu_table(otu, taxa_are_rows = TRUE),
    phyloseq::tax_table(tax))

  # --- sample_data from the legend columns ----------------------------------
  if (length(meta_names) > 0) {
    sdata <- unique(as.data.frame(df[, c("sample", meta_names)]))
    sdata <- sdata[!duplicated(as.character(sdata$sample)), , drop = FALSE]
    sdata$sample <- as.character(sdata$sample)
    rownames(sdata) <- sdata$sample
    sdata <- sdata[samples, meta_names, drop = FALSE]
    ps_parts[[length(ps_parts) + 1]] <- phyloseq::sample_data(sdata)
  }

  do.call(phyloseq::phyloseq, ps_parts)
}

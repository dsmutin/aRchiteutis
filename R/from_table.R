#' Coerce a table into the canonical aRchiteutis long format
#'
#' The plotting helpers all expect a tidy tibble with columns `taxa`, `clade`,
#' `sample`, `N` (counts), `amount` (fraction of all reads) and `amount_cl`
#' (fraction of classified reads), plus optional legend columns.
#'
#' @param df A data frame that already looks like a [get_counts()] result, or
#'   that uses common aliases (`taxon`/`otu`/`asv` for `taxa`, `rank` for
#'   `clade`, `count`/`reads` for `N`, and so on).
#' @param clade Character. Rank letter to fill in when `clade` is missing.
#'
#' @return A [tibble::tibble] in canonical aRchiteutis shape.
#' @export
as_archi <- function(df, clade = "S") {
  if (is.matrix(df)) {
    return(from_abundance(df, clade = clade))
  }
  df <- tibble::as_tibble(df)
  names(df) <- trimws(names(df))

  alias <- c(
    taxon = "taxa", taxonomy = "taxa", otu = "taxa", asv = "taxa",
    feature = "taxa", name = "taxa", Taxa = "taxa", OTU = "taxa",
    rank = "clade", taxonomy_lvl = "clade", level = "clade", Rank = "clade",
    Sample = "sample", sample_id = "sample", sampleid = "sample",
    reads = "N", count = "N", counts = "N", abundance = "N",
    nreads = "N", new_est_reads = "N",
    rel = "amount", relative = "amount", fraction = "amount",
    fraction_total_reads = "amount", rel_abund = "amount",
    amount_classified = "amount_cl", rel_cl = "amount_cl")

  rename_to <- alias[names(df)]
  keep <- !is.na(rename_to) & !(unname(rename_to) %in% names(df))
  if (any(keep)) {
    names(df)[keep] <- unname(rename_to[keep])
  }

  if (!"taxa" %in% names(df)) {
    stop("`as_archi()` needs a taxa column (or a known alias such as `otu`).")
  }
  if (!"sample" %in% names(df)) {
    stop("`as_archi()` needs a sample column.")
  }
  if (!"clade" %in% names(df)) {
    df$clade <- clade
  } else {
    df$clade <- .rank_to_clade(df$clade)
  }

  if (!"N" %in% names(df)) {
    if ("amount" %in% names(df)) {
      df$N <- as.numeric(df$amount)
    } else {
      stop("`as_archi()` needs counts (`N`) or relative abundances (`amount`).")
    }
  }
  df$N <- as.numeric(df$N)
  df$N[is.na(df$N)] <- 0

  totals <- dplyr::summarise(df, .total = sum(.data$N), .by = "sample")
  df <- dplyr::left_join(df, totals, by = "sample")
  if (!"amount" %in% names(df)) {
    df$amount <- ifelse(df$.total > 0, df$N / df$.total, 0)
  }
  if (!"amount_cl" %in% names(df)) {
    df$amount_cl <- df$amount
  }
  df$.total <- NULL

  df$taxa <- as.character(df$taxa)
  df$clade <- as.character(df$clade)
  df$sample <- as.factor(df$sample)
  df$amount <- as.numeric(df$amount)
  df$amount_cl <- as.numeric(df$amount_cl)

  front <- c("taxa", "clade", "sample", "N", "amount", "amount_cl")
  tibble::as_tibble(df[, c(front, setdiff(names(df), front)), drop = FALSE])
}

#' Map taxonomy rank names to Kraken-style clade letters
#' @noRd
.rank_to_clade <- function(rank) {
  key <- toupper(gsub("[^A-Za-z]", "", as.character(rank)))
  map <- c(
    U = "U", UNCLASSIFIED = "U",
    R = "R", ROOT = "R",
    D = "D", DOMAIN = "D", KINGDOM = "D", K = "D",
    P = "P", PHYLUM = "P",
    C = "C", CLASS = "C",
    O = "O", ORDER = "O",
    F = "F", FAMILY = "F",
    G = "G", GENUS = "G",
    S = "S", SPECIES = "S",
    OTU = "S", ASV = "S", FEATURE = "S", TAXA = "S")
  out <- unname(map[key])
  out[is.na(out) | out == ""] <- "S"
  out
}

#' Join an optional sample legend onto a tidy table
#' @noRd
.join_legend <- function(df, legend) {
  if (is.null(legend) || isFALSE(legend)) return(df)
  if (is.character(legend) && length(legend) == 1L) {
    legend <- utils::read.csv(legend, header = TRUE, stringsAsFactors = TRUE)
  }
  legend <- tibble::as_tibble(legend)
  if (!"sample" %in% names(legend)) {
    if (ncol(legend) < 1) stop("legend has no columns to join")
    # first column is the sample key (possibly used as row names earlier)
    names(legend)[1] <- "sample"
  }
  legend$sample <- as.character(legend$sample)
  df$sample <- as.character(df$sample)
  df <- dplyr::left_join(df, legend, by = "sample")
  df$sample <- as.factor(df$sample)
  df
}

#' Import a taxa-by-sample abundance table
#'
#' Accepts the usual microbiome layouts:
#'
#' * a **wide** numeric matrix / data frame (taxa in rows, samples in columns);
#' * a wide data frame whose first column holds taxon names;
#' * a **long** data frame that already has `taxa` + `sample` (passed through
#'   [as_archi()]).
#'
#' The result is a tidy aRchiteutis table that can be handed straight to the
#' `df2*` plotting functions.
#'
#' @param x Matrix, data frame or tibble of abundances.
#' @param taxa Optional character vector of taxon names (used when `x` is a
#'   bare matrix without row names). Ignored when `x` already has a taxa
#'   column / row names.
#' @param samples Optional character vector of sample names for the columns of
#'   a wide table.
#' @param clade Character. Rank letter stored in the `clade` column (`"S"` by
#'   default). A vector is recycled to the taxa.
#' @param legend Optional data frame or CSV path of sample metadata. Must
#'   contain a `sample` column (or use the first column as the sample key).
#'
#' @return A [tibble::tibble] in canonical aRchiteutis shape.
#'
#' @examples
#' mat <- matrix(c(10, 5, 1, 20, 0, 4), nrow = 2,
#'               dimnames = list(c("Lactobacillus", "Bifidobacterium"),
#'                               c("s1", "s2", "s3")))
#' df <- from_abundance(mat, clade = "G")
#' df2composition(df)
#'
#' @export
from_abundance <- function(x, taxa = NULL, samples = NULL, clade = "S",
                           legend = NULL) {
  if (is.data.frame(x) && all(c("taxa", "sample") %in% names(x))) {
    return(.join_legend(as_archi(x, clade = clade), legend))
  }

  if (is.data.frame(x)) {
    first <- x[[1]]
    numeric_rest <- ncol(x) > 1 && all(vapply(x[-1], is.numeric, logical(1)))
    first_is_name <- numeric_rest && (is.character(first) || is.factor(first))
    if (first_is_name && is.null(taxa)) {
      taxa <- as.character(first)
      x <- as.matrix(x[-1])
    } else {
      rn <- rownames(x)
      x <- as.matrix(data.matrix(x))
      if (is.null(taxa) && !is.null(rn) &&
          !identical(rn, as.character(seq_len(nrow(x))))) {
        taxa <- rn
      }
    }
  } else {
    x <- as.matrix(x)
  }
  storage.mode(x) <- "double"
  x[is.na(x)] <- 0

  if (is.null(taxa)) {
    taxa <- rownames(x)
    if (is.null(taxa)) taxa <- paste0("taxon", seq_len(nrow(x)))
  }
  if (is.null(samples)) {
    samples <- colnames(x)
    if (is.null(samples)) samples <- paste0("sample", seq_len(ncol(x)))
  }
  if (length(taxa) != nrow(x)) {
    stop("`taxa` length (", length(taxa), ") != number of rows (", nrow(x), ")")
  }
  if (length(samples) != ncol(x)) {
    stop("`samples` length (", length(samples),
         ") != number of columns (", ncol(x), ")")
  }

  wide <- as.data.frame(x, stringsAsFactors = FALSE)
  names(wide) <- samples
  wide <- tibble::add_column(tibble::as_tibble(wide), taxa = taxa, .before = 1)
  long <- tidyr::pivot_longer(
    wide, cols = -"taxa", names_to = "sample", values_to = "N")

  cl <- rep(clade, length.out = length(taxa))
  long$clade <- cl[match(long$taxa, taxa)]
  .join_legend(as_archi(long, clade = clade[1]), legend)
}

#' Import a phyloseq object
#'
#' Pulls the OTU table, optional taxonomy table and sample data out of a
#' phyloseq object and returns a tidy aRchiteutis table. Requires the
#' Bioconductor phyloseq package at runtime (not imported).
#'
#' A plain list with elements `otu_table` (taxa x samples matrix), optional
#' `tax_table` and optional `sample_data` is also accepted, which is handy in
#' tests and when the phyloseq object has already been unpacked in Python.
#'
#' @param physeq A `phyloseq` object, or a list with `otu_table` / `tax_table`
#'   / `sample_data`.
#' @param taxa_rank Optional rank name (`"Genus"`, `"Species"`, `"Family"`, …)
#'   or clade letter used to label `taxa` and `clade`. When `NULL`, taxon
#'   names from the OTU table are kept and the finest available rank is used
#'   for `clade`.
#'
#' @return A [tibble::tibble] in canonical aRchiteutis shape.
#' @export
from_phyloseq <- function(physeq, taxa_rank = NULL) {
  unpacked <- .unpack_phyloseq(physeq)
  otu <- unpacked$otu
  tax <- unpacked$tax
  sad <- unpacked$sad

  taxa <- rownames(otu)
  clade <- "S"
  if (!is.null(tax)) {
    rank_cols <- colnames(tax)
    pick <- NULL
    if (!is.null(taxa_rank)) {
      want <- toupper(taxa_rank)
      pick <- which(toupper(rank_cols) == want | .rank_to_clade(rank_cols) ==
                      .rank_to_clade(taxa_rank))
      pick <- if (length(pick)) pick[[1]] else NULL
    }
    if (is.null(pick)) {
      # finest (right-most) non-empty rank per taxon
      labels <- apply(tax, 1, function(row) {
        row <- as.character(row)
        row <- row[!is.na(row) & nzchar(row)]
        if (!length(row)) NA_character_ else utils::tail(row, 1)
      })
      if (is.null(taxa_rank)) {
        taxa <- ifelse(is.na(labels), taxa, labels)
      }
      last_rank <- apply(tax, 1, function(row) {
        ok <- !is.na(row) & nzchar(as.character(row))
        if (!any(ok)) "S" else colnames(tax)[max(which(ok))]
      })
      clade <- .rank_to_clade(last_rank)
    } else {
      labels <- as.character(tax[, pick])
      taxa <- ifelse(is.na(labels) | !nzchar(labels), rownames(otu), labels)
      clade <- .rank_to_clade(rank_cols[pick])
    }
  } else if (!is.null(taxa_rank)) {
    clade <- .rank_to_clade(taxa_rank)
  }

  from_abundance(otu, taxa = taxa, samples = colnames(otu),
                 clade = clade, legend = sad)
}

#' @noRd
.unpack_phyloseq <- function(physeq) {
  if (inherits(physeq, "phyloseq")) {
    if (!requireNamespace("phyloseq", quietly = TRUE)) {
      stop("Install phyloseq (Bioconductor) to import phyloseq objects.")
    }
    otu <- as.matrix(phyloseq::otu_table(physeq))
    if (!isTRUE(phyloseq::taxa_are_rows(physeq))) {
      otu <- t(otu)
    }
    tax <- tryCatch(as.data.frame(phyloseq::tax_table(physeq),
                                  stringsAsFactors = FALSE),
                    error = function(e) NULL)
    sad <- tryCatch({
      sd <- as.data.frame(phyloseq::sample_data(physeq),
                          stringsAsFactors = TRUE)
      sd$sample <- rownames(sd)
      sd
    }, error = function(e) NULL)
    return(list(otu = otu, tax = tax, sad = sad))
  }

  if (is.list(physeq) && !is.null(physeq$otu_table)) {
    otu <- as.matrix(physeq$otu_table)
    if (!is.null(physeq$taxa_are_rows) && isFALSE(physeq$taxa_are_rows)) {
      otu <- t(otu)
    }
    tax <- physeq$tax_table
    if (!is.null(tax)) tax <- as.data.frame(tax, stringsAsFactors = FALSE)
    sad <- physeq$sample_data
    if (!is.null(sad) && !"sample" %in% names(sad)) {
      sad <- as.data.frame(sad)
      sad$sample <- rownames(sad)
    }
    return(list(otu = otu, tax = tax, sad = sad))
  }

  stop("`from_phyloseq()` expects a phyloseq object or a list with $otu_table.")
}

#' Transfer a Python-loaded dataset into aRchiteutis
#'
#' Small bridge for the "load in Python, visualise in R" step:
#'
#' 1. From Python, dump a pandas DataFrame / numpy array with
#'    `inst/python/python2r.py` (writes a wide CSV, taxa in the first column).
#' 2. In R, call `python2r("abundance.csv")` and then any `df2*` plot.
#'
#' Live `reticulate` objects (pandas DataFrame, numpy ndarray) are converted
#' in-memory when \pkg{reticulate} is installed.
#'
#' @param x Path to a CSV/TSV written by the Python helper, an in-memory
#'   abundance table, or a reticulate Python object.
#' @param ... Passed to [from_abundance()] (`clade`, `legend`, `taxa`,
#'   `samples`).
#'
#' @return A [tibble::tibble] in canonical aRchiteutis shape.
#'
#' @examples
#' csv <- system.file("extdata", "python_abundance.csv", package = "aRchiteutis")
#' df <- python2r(csv, clade = "G")
#' df2barplot(df)
#'
#' @export
python2r <- function(x, ...) {
  if (inherits(x, "python.builtin.object")) {
    if (!requireNamespace("reticulate", quietly = TRUE)) {
      stop("Install reticulate to convert live Python objects with python2r().")
    }
    return(from_abundance(reticulate::py_to_r(x), ...))
  }

  if (is.character(x) && length(x) == 1L) {
    if (!file.exists(x)) {
      stop("python2r(): file not found: ", x)
    }
    ext <- tolower(tools::file_ext(x))
    if (ext %in% c("rds")) {
      return(from_abundance(readRDS(x), ...))
    }
    sep <- if (ext %in% c("tsv", "txt")) "\t" else ","
    tab <- utils::read.table(x, header = TRUE, sep = sep, check.names = FALSE,
                             stringsAsFactors = FALSE, comment.char = "")
    return(from_abundance(tab, ...))
  }

  from_abundance(x, ...)
}

#' Path to the bundled Python exporter
#'
#' @return Absolute path of `inst/python/python2r.py`.
#' @export
python2r_script <- function() {
  system.file("python", "python2r.py", package = "aRchiteutis", mustWork = TRUE)
}

# Classifier reports -> phyloseq (or a phyloseq-shaped list when phyloseq
# is not installed). Lineage walking and host-taxon cleanup follow the
# metagenomics harness bracken/kraken parsers.

archi_rank_code <- function() {
  c(D = "kingdom", K = "kingdom", P = "phylum", C = "class",
    O = "order", F = "family", G = "genus", S = "species")
}

archi_host_name <- function(name) {
  grepl("Chordata|Mitochondria|Chloroplast", name, ignore.case = TRUE)
}

#' Read one Kraken, Kraken2, KrakenUniq or Bracken file
#'
#' Kraken-style reports use six columns (percentage, clade reads, direct
#' reads, rank, taxid, indented name). KrakenUniq adds k-mer columns before
#' the taxid. Bracken genus tables have a header with `name` and
#' `new_est_reads`.
#'
#' @param path File path.
#' @param rank Rank code to keep for Kraken reports, usually `"G"` or `"S"`.
#' @return A data frame with `sample`, `taxid`, `name`, `rank`, `reads`.
#' @export
read_classifier_report <- function(path, rank = "G") {
  sample <- basename(path)
  sample <- sub("\\.(txt|tsv|report|kreport|bracken)$", "", sample, ignore.case = TRUE)
  first <- readLines(path, n = 1L, warn = FALSE)
  if (grepl("new_est_reads", first) || grepl("^name\t", first)) {
    return(archi_read_bracken(path, sample))
  }
  df <- utils::read.table(path, sep = "\t", quote = "", fill = TRUE,
                          comment.char = "", header = FALSE, stringsAsFactors = FALSE)
  if (ncol(df) < 6L) stop("Kraken-style report needs at least 6 columns: ", path, call. = FALSE)
  # KrakenUniq: pct, reads, taxReads, kmers, dup, taxID, rank, name
  if (ncol(df) >= 8L && grepl("^[A-Z][0-9]?$", as.character(df[[7]][1]))) {
    name <- trimws(df[[ncol(df)]])
    out <- data.frame(
      taxid = as.integer(df[[6]]),
      rank = sub("[0-9]+$", "", as.character(df[[7]])),
      reads = as.numeric(df[[2]]),
      name = name,
      depth = nchar(df[[ncol(df)]]) - nchar(name),
      stringsAsFactors = FALSE
    )
  } else {
    name <- trimws(df[[6]])
    out <- data.frame(
      taxid = as.integer(df[[5]]),
      rank = sub("[0-9]+$", "", as.character(df[[4]])),
      reads = as.numeric(df[[2]]),
      name = name,
      depth = nchar(as.character(df[[6]])) - nchar(name),
      stringsAsFactors = FALSE
    )
  }
  out <- out[!is.na(out$taxid) & out$taxid > 0L, , drop = FALSE]
  archi_lineage_from_kraken(out, sample = sample, rank = rank)
}

archi_read_bracken <- function(path, sample) {
  df <- utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE, quote = "")
  if (!all(c("name", "new_est_reads", "taxonomy_id") %in% names(df))) {
    stop("Bracken table needs name, new_est_reads, taxonomy_id: ", path, call. = FALSE)
  }
  rank <- if ("taxonomy_lvl" %in% names(df)) as.character(df$taxonomy_lvl) else "G"
  data.frame(
    sample = sample,
    taxid = as.integer(df$taxonomy_id),
    name = as.character(df$name),
    rank = sub("[0-9]+$", "", rank),
    reads = as.numeric(df$new_est_reads),
    kingdom = NA_character_, phylum = NA_character_, class = NA_character_,
    order = NA_character_, family = NA_character_,
    genus = ifelse(sub("[0-9]+$", "", rank) == "G", as.character(df$name), NA_character_),
    species = ifelse(sub("[0-9]+$", "", rank) == "S", as.character(df$name), NA_character_),
    stringsAsFactors = FALSE
  )
}

archi_lineage_from_kraken <- function(out, sample, rank) {
  codes <- archi_rank_code()
  ranks <- archi_rank_cols()
  stack_depth <- integer(0)
  stack_rank <- character(0)
  stack_name <- character(0)
  rows <- list()
  k <- 0L
  for (i in seq_len(nrow(out))) {
    d <- out$depth[[i]]
    while (length(stack_depth) && stack_depth[[length(stack_depth)]] >= d) {
      n <- length(stack_depth)
      stack_depth <- stack_depth[-n]
      stack_rank <- stack_rank[-n]
      stack_name <- stack_name[-n]
    }
    code <- out$rank[[i]]
    if (!is.na(code) && code %in% names(codes)) {
      stack_depth <- c(stack_depth, d)
      stack_rank <- c(stack_rank, codes[[code]])
      stack_name <- c(stack_name, out$name[[i]])
    }
    if (!is.na(code) && code %in% rank && out$reads[[i]] > 0) {
      lin <- stats::setNames(rep(NA_character_, length(ranks)), ranks)
      if (length(stack_rank)) lin[stack_rank] <- stack_name
      k <- k + 1L
      rows[[k]] <- data.frame(
        sample = sample, taxid = out$taxid[[i]], name = out$name[[i]],
        rank = code, reads = out$reads[[i]],
        as.list(lin), stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    stop("No rows at rank ", paste(rank, collapse = ","), " in ", sample, call. = FALSE)
  }
  do.call(rbind, rows)
}

archi_read_legend <- function(legend, trim_char = FALSE) {
  if (isFALSE(legend) || is.null(legend)) return(NULL)
  df <- utils::read.csv(legend, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
  id_col <- intersect(c("sample-id", "sample_id", "sampleID", "SampleID", "sample"), names(df))
  if (length(id_col)) {
    ids <- as.character(df[[id_col[[1]]]])
    df[[id_col[[1]]]] <- NULL
  } else {
    ids <- rownames(utils::read.csv(legend, header = TRUE, row.names = 1, check.names = FALSE))
    df <- utils::read.csv(legend, header = TRUE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
  }
  if (!isFALSE(trim_char)) {
    ids <- vapply(strsplit(ids, trim_char, fixed = TRUE), function(z) z[[1]], character(1))
  }
  rownames(df) <- ids
  df
}

archi_assemble_phyloseq <- function(long, legend = NULL) {
  long <- long[!archi_host_name(long$name) & !long$taxid %in% c(9606L, 9605L, 33208L), , drop = FALSE]
  if (!nrow(long)) stop("No taxa left after host/organelle filtering", call. = FALSE)
  long$taxa_id <- paste0("tax_", long$taxid)
  samples <- unique(long$sample)
  taxa <- unique(long$taxa_id)
  counts <- matrix(0, nrow = length(taxa), ncol = length(samples),
                   dimnames = list(taxa, samples))
  for (i in seq_len(nrow(long))) {
    counts[long$taxa_id[[i]], long$sample[[i]]] <-
      counts[long$taxa_id[[i]], long$sample[[i]]] + long$reads[[i]]
  }
  counts <- counts[rowSums(counts) > 0, , drop = FALSE]
  meta_taxa <- long[!duplicated(long$taxa_id), , drop = FALSE]
  meta_taxa <- meta_taxa[match(rownames(counts), meta_taxa$taxa_id), , drop = FALSE]
  ranks <- archi_rank_cols()
  tax <- meta_taxa[, ranks, drop = FALSE]
  tax <- fill_na_last_classified(tax, ranks)
  rownames(tax) <- meta_taxa$taxa_id
  tree <- ranks_to_tree(data.frame(
    tax, taxa_id = rownames(tax), taxid = meta_taxa$taxid,
    tip_name = rownames(tax), stringsAsFactors = FALSE
  ))
  sam <- if (is.null(legend)) {
    data.frame(sample = colnames(counts), row.names = colnames(counts), stringsAsFactors = FALSE)
  } else {
    legend <- legend[intersect(colnames(counts), rownames(legend)), , drop = FALSE]
    if (!nrow(legend)) {
      data.frame(sample = colnames(counts), row.names = colnames(counts), stringsAsFactors = FALSE)
    } else {
      missing <- setdiff(colnames(counts), rownames(legend))
      if (length(missing)) {
        extra <- as.data.frame(matrix(NA, nrow = length(missing), ncol = ncol(legend),
                                      dimnames = list(missing, names(legend))))
        legend <- rbind(legend, extra)
      }
      legend[colnames(counts), , drop = FALSE]
    }
  }
  archi_phyloseq_object(counts, as.matrix(tax), sam, tree)
}

archi_phyloseq_object <- function(counts, tax, sam, tree) {
  if (requireNamespace("phyloseq", quietly = TRUE)) {
    return(phyloseq::phyloseq(
      phyloseq::otu_table(counts, taxa_are_rows = TRUE),
      phyloseq::tax_table(tax),
      phyloseq::sample_data(sam),
      phyloseq::phy_tree(tree)
    ))
  }
  structure(list(
    otu_table = counts,
    tax_table = tax,
    sample_data = sam,
    phy_tree = tree
  ), class = "archi_phyloseq")
}

#' Import Kraken, Kraken2, KrakenUniq or Bracken reports as phyloseq
#'
#' @param path Directory of reports, or a character vector of files.
#' @param pattern Regular expression selecting files when `path` is a directory.
#' @param rank Rank code kept in the OTU table (`"G"` or `"S"`).
#' @param legend Path to a CSV legend. A QIIME2 manifest (`sample-id`) or the
#'   package legend (sample id in the first column) are both accepted.
#' @param trim_char Passed through when matching legend ids to file names.
#'   File names are trimmed the same way before they become sample ids.
#' @return A `phyloseq` object, or an `archi_phyloseq` list with
#'   `otu_table`, `tax_table`, `sample_data` and `phy_tree` when \pkg{phyloseq}
#'   is not installed.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
#' ps <- kraken_to_phyloseq(path, pattern = "m11_.*k2", legend = legend,
#'                          trim_char = "_", rank = "G")
#' if (inherits(ps, "phyloseq")) phyloseq::ntaxa(ps) else nrow(ps$otu_table)
#'
#' @export
kraken_to_phyloseq <- function(path, pattern = "", rank = "G",
                              legend = NULL, trim_char = FALSE) {
  files <- if (length(path) == 1L && dir.exists(path)) {
    list.files(path, pattern = pattern, full.names = TRUE)
  } else {
    path
  }
  if (!length(files)) stop("No classifier reports matched", call. = FALSE)
  long <- lapply(files, function(f) {
    rec <- read_classifier_report(f, rank = rank)
    if (!isFALSE(trim_char)) {
      rec$sample <- strsplit(rec$sample, trim_char, fixed = TRUE)[[1]][[1]]
    }
    rec
  })
  long <- do.call(rbind, long)
  archi_assemble_phyloseq(long, legend = archi_read_legend(legend, trim_char))
}

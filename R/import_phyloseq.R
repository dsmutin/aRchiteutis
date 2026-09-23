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

#' Import Kaiju output or a kaiju2table summary as phyloseq
#'
#' Accepts either a per-read `kaiju.out` (`C`/`U`, read name, taxid, optional
#' lineage) or a `kaiju2table` summary (`file`, `percent`, `reads`,
#' `taxon_id`, `taxon_name`). Several files are different samples. Lineage
#' text separated by `"; "` fills rank columns; otherwise the taxon name is
#' stored at genus or species and a rank tree is still attached.
#'
#' @param path Directory or character vector of Kaiju files.
#' @param pattern File filter when `path` is a directory.
#' @param legend Optional sample legend. See [kraken_to_phyloseq()].
#' @param trim_char Trim file names to sample ids.
#' @return A phyloseq object, or an `archi_phyloseq` list.
#'
#' @examples
#' kaiju <- system.file("extdata", "kaiju-summary-bee.tsv", package = "aRchiteutis")
#' ps <- kaiju_to_phyloseq(kaiju)
#' if (inherits(ps, "phyloseq")) phyloseq::ntaxa(ps) else nrow(ps$otu_table)
#'
#' @export
kaiju_to_phyloseq <- function(path, pattern = "", legend = NULL, trim_char = FALSE) {
  files <- if (length(path) == 1L && dir.exists(path)) {
    list.files(path, pattern = pattern, full.names = TRUE)
  } else {
    path
  }
  if (!length(files)) stop("No Kaiju files matched", call. = FALSE)
  long <- do.call(rbind, lapply(files, archi_read_kaiju_file))
  if (!isFALSE(trim_char)) {
    long$sample <- vapply(strsplit(as.character(long$sample), trim_char, fixed = TRUE),
                          function(z) z[[1]], character(1))
  }
  archi_assemble_phyloseq(long, legend = archi_read_legend(legend, trim_char))
}

archi_read_kaiju_file <- function(path) {
  header <- readLines(path, n = 1L, warn = FALSE)
  sample <- sub("\\.(tsv|txt|out|csv)$", "", basename(path), ignore.case = TRUE)
  if (grepl("taxon_id|taxon_name", header)) {
    df <- utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE, quote = "")
    name_col <- intersect(c("taxon_name", "taxon"), names(df))[[1]]
    id_col <- intersect(c("taxon_id", "taxid"), names(df))[[1]]
    if ("file" %in% names(df)) sample <- sub("\\.(tsv|txt|out)$", "", df$file[[1]])
    return(archi_kaiju_names_to_long(df[[id_col]], df[[name_col]], df$reads, sample))
  }
  df <- utils::read.table(path, sep = "\t", quote = "", fill = TRUE,
                          comment.char = "", stringsAsFactors = FALSE)
  if (ncol(df) >= 4L && any(grepl(";", df[[4]]))) {
    ok <- df[[1]] == "C"
    return(archi_kaiju_names_to_long(df[[3]][ok], df[[4]][ok], rep(1, sum(ok)), sample))
  }
  ok <- df[[1]] == "C"
  archi_kaiju_names_to_long(df[[3]][ok], rep(NA_character_, sum(ok)), rep(1, sum(ok)), sample)
}

archi_kaiju_names_to_long <- function(taxid, name, reads, sample) {
  taxid <- as.integer(taxid)
  name <- as.character(name)
  reads <- as.numeric(reads)
  keep <- !is.na(taxid) & taxid > 0L & reads > 0
  taxid <- taxid[keep]
  name <- name[keep]
  reads <- reads[keep]
  # Aggregate per-read classifications.
  key <- paste(taxid, name, sep = "\t")
  agg <- rowsum(reads, key)
  parts <- strsplit(rownames(agg), "\t", fixed = TRUE)
  taxid <- as.integer(vapply(parts, `[[`, character(1), 1))
  name <- vapply(parts, function(z) if (length(z) > 1) z[[2]] else NA_character_, character(1))
  ranks <- archi_rank_cols()
  rows <- lapply(seq_along(taxid), function(i) {
    lin <- stats::setNames(rep(NA_character_, length(ranks)), ranks)
    nm <- name[[i]]
    if (!is.na(nm) && grepl(";", nm)) {
      bits <- trimws(strsplit(nm, ";", fixed = TRUE)[[1]])
      bits <- bits[nzchar(bits)]
      use <- utils::tail(bits, length(ranks))
      lin[seq_len(length(use))] <- use
    } else if (!is.na(nm) && nzchar(nm)) {
      if (grepl(" ", nm)) lin[["species"]] <- nm else lin[["genus"]] <- nm
    }
    data.frame(sample = sample, taxid = taxid[[i]], name = if (is.na(nm) || !nzchar(nm)) paste0("tax_", taxid[[i]]) else nm,
               rank = if (!is.na(lin[["species"]])) "S" else "G",
               reads = agg[[i]], as.list(lin), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Import QIIME 2 feature table, taxonomy and tree artifacts as phyloseq
#'
#' `features` may be a feature-table `.qza` or a directory that contains one.
#' Taxonomy and a rooted tree are optional `.qza` files. Metadata is a QIIME 2
#' manifest (TSV or CSV): a `sample-id` / `SampleID` column, or the first
#' column, plus any annotation columns such as `target`. When \pkg{qiime2R} is
#' installed it is tried first. Otherwise a feature-table TSV inside the
#' archive is read directly, and a BIOM table is read with \pkg{biomformat}.
#' Chloroplast, mitochondria and Chordata features are dropped. A Newick tree
#' is attached when its tips match the feature ids; otherwise a rank-formula
#' tree is built from the taxonomy table.
#'
#' @param features Feature-table `.qza`, an exported feature-table TSV, or a
#'   directory of artifacts.
#' @param taxonomy Taxonomy `.qza` or TSV. Discovered next to `features` when
#'   that argument is a directory and this is `NULL`.
#' @param metadata Sample metadata TSV/CSV, or a `.qza`. Same discovery rule.
#' @param tree Rooted-tree `.qza` or Newick. Same discovery rule.
#' @return A `phyloseq` object, or an `archi_phyloseq` list.
#'
#' @examples
#' qza <- system.file("extdata", "qza", package = "aRchiteutis")
#' if (requireNamespace("biomformat", quietly = TRUE)) {
#'   ps <- qza_to_phyloseq(qza)
#'   if (inherits(ps, "phyloseq")) phyloseq::nsamples(ps) else ncol(ps$otu_table)
#' }
#'
#' @export
qza_to_phyloseq <- function(features, taxonomy = NULL, metadata = NULL, tree = NULL) {
  if (length(features) == 1L && dir.exists(features)) {
    found <- archi_discover_qza(features)
    if (is.null(taxonomy)) taxonomy <- found$taxonomy
    if (is.null(metadata)) metadata <- found$metadata
    if (is.null(tree)) tree <- found$tree
    features <- found$features
  }
  if (is.null(features) || !nzchar(features)) {
    stop("No feature table (.qza or TSV) found", call. = FALSE)
  }
  if (requireNamespace("qiime2R", quietly = TRUE) && grepl("\\.qza$", features, ignore.case = TRUE)) {
    args <- list(features = features)
    if (!is.null(taxonomy)) args$taxonomy <- taxonomy
    if (!is.null(metadata)) args$metadata <- metadata
    if (!is.null(tree)) args$tree <- tree
    ps <- tryCatch(do.call(qiime2R::qza_to_phyloseq, args), error = function(e) NULL)
    if (!is.null(ps)) return(archi_finish_qiime2r(ps))
  }
  otu <- archi_read_feature_table(features)
  tax <- if (is.null(taxonomy)) {
    matrix(NA_character_, nrow = nrow(otu), ncol = length(archi_rank_cols()),
           dimnames = list(rownames(otu), archi_rank_cols()))
  } else {
    archi_read_qiime_taxonomy(taxonomy, rownames(otu))
  }
  drop <- apply(tax, 1, function(row) {
    any(grepl("Chloroplast|Mitochondria|Chordata", row, ignore.case = TRUE))
  })
  if (any(drop)) {
    otu <- otu[!drop, , drop = FALSE]
    tax <- tax[!drop, , drop = FALSE]
  }
  keep <- rowSums(otu) > 0
  otu <- otu[keep, , drop = FALSE]
  tax <- tax[keep, , drop = FALSE]
  tax_df <- as.data.frame(tax, stringsAsFactors = FALSE)
  tax_df <- fill_na_last_classified(tax_df, archi_rank_cols())
  rownames(tax_df) <- rownames(otu)
  sam <- archi_align_metadata(otu, metadata)
  phy <- archi_read_newick(tree)
  phy <- archi_align_tree(phy, rownames(otu))
  if (is.null(phy)) {
    phy <- ranks_to_tree(data.frame(
      tax_df, taxa_id = rownames(tax_df), tip_name = rownames(tax_df),
      stringsAsFactors = FALSE
    ))
  }
  archi_phyloseq_object(otu, as.matrix(tax_df), sam, phy)
}

archi_discover_qza <- function(dir) {
  files <- list.files(dir, full.names = TRUE)
  bn <- basename(files)
  pick <- function(pat) {
    hit <- files[grepl(pat, bn, ignore.case = TRUE)]
    if (length(hit)) hit[[1]] else NULL
  }
  list(
    features = pick("table.*\\.qza$|feature.*\\.qza$|feature-table\\.tsv$"),
    taxonomy = pick("taxonom.*\\.(qza|tsv|txt)$"),
    tree = pick("tree.*\\.(qza|nwk|tre|newick)$"),
    metadata = pick("metadata.*\\.(tsv|csv|txt)$|manifest.*\\.(tsv|csv)$")
  )
}

archi_qza_extract <- function(path) {
  if (!grepl("\\.qza$", path, ignore.case = TRUE)) return(path)
  dest <- file.path(tempdir(), paste0("archi-qza-", tools::file_path_sans_ext(basename(path))))
  if (!dir.exists(dest)) utils::unzip(path, exdir = dest)
  dest
}

archi_qza_data_file <- function(path, pattern) {
  root <- archi_qza_extract(path)
  if (!dir.exists(root)) {
    if (grepl(pattern, basename(root), ignore.case = TRUE)) return(root)
    stop("Cannot find data matching ", pattern, " in ", path, call. = FALSE)
  }
  files <- list.files(root, recursive = TRUE, full.names = TRUE)
  # Prefer the artifact payload over provenance copies.
  data <- files[grepl("/data/", files, fixed = TRUE) & grepl(pattern, basename(files), ignore.case = TRUE)]
  if (!length(data)) data <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
  if (!length(data)) stop("No file matching ", pattern, " inside ", path, call. = FALSE)
  data[[1]]
}

archi_read_feature_table <- function(path) {
  if (dir.exists(path)) {
    path <- archi_discover_qza(path)$features
    if (is.null(path)) stop("No feature table in directory", call. = FALSE)
  }
  if (grepl("\\.qza$", path, ignore.case = TRUE)) {
    biom <- tryCatch(
      archi_qza_data_file(path, "feature-table\\.biom$|\\.biom$"),
      error = function(e) NULL
    )
    tsv <- tryCatch(
      archi_qza_data_file(path, "feature-table\\.tsv$|\\.tsv$"),
      error = function(e) NULL
    )
    if (!is.null(biom)) return(archi_read_biom(biom))
    if (!is.null(tsv)) return(archi_read_feature_tsv(tsv))
    stop("Feature table .qza has neither BIOM nor TSV data: ", path, call. = FALSE)
  }
  if (grepl("\\.biom$", path, ignore.case = TRUE)) return(archi_read_biom(path))
  archi_read_feature_tsv(path)
}

archi_read_biom <- function(path) {
  if (!requireNamespace("biomformat", quietly = TRUE)) {
    stop("Reading a BIOM feature table needs the biomformat package", call. = FALSE)
  }
  mat <- as.matrix(biomformat::biom_data(suppressWarnings(biomformat::read_biom(path))))
  storage.mode(mat) <- "double"
  mat[is.na(mat)] <- 0
  mat
}

archi_read_feature_tsv <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl("^#", lines)]
  sep <- if (any(grepl("\t", lines[[1]]))) "\t" else ","
  df <- utils::read.delim(text = paste(lines, collapse = "\n"), sep = sep,
                          check.names = FALSE, stringsAsFactors = FALSE, quote = "")
  ids <- as.character(df[[1]])
  df[[1]] <- NULL
  mat <- as.matrix(data.frame(lapply(df, as.numeric), check.names = FALSE, row.names = ids))
  storage.mode(mat) <- "double"
  mat[is.na(mat)] <- 0
  mat
}

archi_read_qiime_taxonomy <- function(path, taxa_ids) {
  if (grepl("\\.qza$", path, ignore.case = TRUE)) {
    path <- archi_qza_data_file(path, "taxonomy\\.tsv$|taxonomy\\.txt$")
  }
  df <- utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE, quote = "")
  id_col <- intersect(c("Feature ID", "FeatureID", "OTU", "#OTU ID", "ASV"), names(df))
  tax_col <- intersect(c("Taxon", "taxonomy"), names(df))
  ranks <- archi_rank_cols()
  if (length(id_col) && length(tax_col)) {
    parsed <- lapply(df[[tax_col[[1]]]], archi_parse_qiime_taxon)
    tax <- do.call(rbind, parsed)
    rownames(tax) <- as.character(df[[id_col[[1]]]])
  } else if (all(ranks %in% tolower(names(df)))) {
    names(df) <- tolower(names(df))
    tax <- as.matrix(df[, ranks, drop = FALSE])
    id_col <- setdiff(names(df), ranks)[[1]]
    rownames(tax) <- as.character(df[[id_col]])
  } else {
    stop("Unrecognized taxonomy table: ", path, call. = FALSE)
  }
  missing <- setdiff(taxa_ids, rownames(tax))
  if (length(missing)) {
    fill <- matrix(NA_character_, nrow = length(missing), ncol = ncol(tax),
                   dimnames = list(missing, colnames(tax)))
    tax <- rbind(tax, fill)
  }
  tax[taxa_ids, , drop = FALSE]
}

archi_parse_qiime_taxon <- function(taxon) {
  ranks <- archi_rank_cols()
  out <- stats::setNames(rep(NA_character_, length(ranks)), ranks)
  parts <- trimws(strsplit(as.character(taxon), ";", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  map <- c(d = "kingdom", k = "kingdom", p = "phylum", c = "class",
           o = "order", f = "family", g = "genus", s = "species")
  prefixed <- FALSE
  for (part in parts) {
    m <- regmatches(part, regexec("^([dkpcofgs])__(.*)$", part, perl = TRUE))[[1]]
    if (length(m) >= 3) {
      prefixed <- TRUE
      val <- trimws(m[[3]])
      if (nzchar(val)) out[[map[[m[[2]]]]]] <- val
    }
  }
  if (!prefixed && length(parts)) {
    use <- utils::tail(parts, length(ranks))
    out[seq.int(length(ranks) - length(use) + 1L, length(ranks))] <- use
  }
  out
}

archi_read_sample_metadata <- function(path) {
  if (grepl("\\.qza$", path, ignore.case = TRUE)) {
    path <- archi_qza_data_file(path, "metadata.*\\.(tsv|csv|txt)$|sample.*\\.(tsv|csv|txt)$")
  }
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl("^#q2:types", lines, ignore.case = TRUE)]
  lines <- lines[nzchar(lines)]
  if (grepl("^#", lines[[1]])) lines[[1]] <- sub("^#+", "", lines[[1]])
  sep <- if (any(grepl("\t", lines[[1]]))) "\t" else ","
  df <- utils::read.delim(text = paste(lines, collapse = "\n"), sep = sep,
                          check.names = FALSE, stringsAsFactors = FALSE, quote = "")
  id_col <- intersect(c("sample-id", "sample_id", "sampleID", "SampleID", "sample", "SampleID"), names(df))
  if (!length(id_col)) id_col <- names(df)[[1]]
  ids <- as.character(df[[id_col[[1]]]])
  df[[id_col[[1]]]] <- NULL
  rownames(df) <- ids
  df
}

archi_align_metadata <- function(otu, metadata) {
  if (is.null(metadata)) {
    return(data.frame(sample = colnames(otu), row.names = colnames(otu), stringsAsFactors = FALSE))
  }
  legend <- archi_read_sample_metadata(metadata)
  legend <- legend[intersect(colnames(otu), rownames(legend)), , drop = FALSE]
  missing <- setdiff(colnames(otu), rownames(legend))
  if (length(missing) && ncol(legend)) {
    extra <- as.data.frame(matrix(NA, nrow = length(missing), ncol = ncol(legend),
                                  dimnames = list(missing, names(legend))),
                           stringsAsFactors = FALSE)
    legend <- rbind(legend, extra)
  }
  if (!nrow(legend)) {
    return(data.frame(sample = colnames(otu), row.names = colnames(otu), stringsAsFactors = FALSE))
  }
  legend[colnames(otu), , drop = FALSE]
}

archi_read_newick <- function(tree) {
  if (is.null(tree)) return(NULL)
  if (inherits(tree, "phylo")) return(tree)
  path <- tree
  if (grepl("\\.qza$", path, ignore.case = TRUE)) {
    path <- archi_qza_data_file(path, "\\.nwk$|\\.tre$|\\.newick$|tree\\.txt$")
  }
  ape::read.tree(path)
}

archi_align_tree <- function(tree, ids) {
  if (is.null(tree)) return(NULL)
  extra <- setdiff(tree$tip.label, ids)
  if (length(extra)) tree <- ape::drop.tip(tree, extra)
  if (!setequal(tree$tip.label, ids)) return(NULL)
  tree
}

archi_finish_qiime2r <- function(ps) {
  if (!requireNamespace("phyloseq", quietly = TRUE)) return(ps)
  otu <- as(phyloseq::otu_table(ps), "matrix")
  if (!phyloseq::taxa_are_rows(ps)) otu <- t(otu)
  tax <- tryCatch(as.matrix(phyloseq::tax_table(ps)), error = function(e) NULL)
  if (!is.null(tax)) {
    drop <- apply(tax, 1, function(row) {
      any(grepl("Chloroplast|Mitochondria|Chordata", row, ignore.case = TRUE))
    })
    if (any(drop)) {
      ps <- phyloseq::prune_taxa(!drop, ps)
      otu <- otu[!drop, , drop = FALSE]
      tax <- tax[!drop, , drop = FALSE]
    }
  }
  tr <- tryCatch(phyloseq::phy_tree(ps), error = function(e) NULL)
  aligned <- archi_align_tree(tr, rownames(otu))
  if (is.null(aligned) && !is.null(tax)) {
    tax_df <- as.data.frame(tax, stringsAsFactors = FALSE)
    names(tax_df) <- tolower(names(tax_df))
    for (rk in setdiff(archi_rank_cols(), names(tax_df))) tax_df[[rk]] <- NA_character_
    tax_df <- fill_na_last_classified(tax_df[, archi_rank_cols(), drop = FALSE])
    rownames(tax_df) <- rownames(otu)
    phy <- ranks_to_tree(data.frame(
      tax_df, taxa_id = rownames(tax_df), tip_name = rownames(tax_df),
      stringsAsFactors = FALSE
    ))
    phyloseq::phy_tree(ps) <- phyloseq::phy_tree(phy)
  }
  ps
}

#' Import an abundance table keyed by NCBI taxid as phyloseq
#'
#' Rows are taxids (row names, or a `taxid` / `taxonomy_id` / `taxon_id`
#' column). Other columns are samples. Lineages come from
#' [taxids_to_lineage()]; pass `xml` to stay offline. A rank-formula tree is
#' attached. Host and organelle taxids are dropped.
#'
#' @param counts Matrix, data frame, or path to a TSV/CSV.
#' @param metadata Optional sample legend. QIIME 2 `sample-id` or a first-column
#'   sample id, plus a `target` column when you have one.
#' @param xml Optional NCBI taxonomy XML. `NULL` fetches lineages from NCBI.
#' @param trim_char Split legend sample ids on this character before matching
#'   them to column names.
#' @return A `phyloseq` object, or an `archi_phyloseq` list.
#'
#' @examples
#' counts <- system.file("extdata", "abundance-taxid-bee.tsv", package = "aRchiteutis")
#' xml <- paste(readLines(system.file("extdata", "ncbi_taxonomy.xml",
#'                                    package = "aRchiteutis"), warn = FALSE),
#'              collapse = "\n")
#' legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
#' ps <- abundance_taxid_to_phyloseq(counts, metadata = legend, xml = xml,
#'                                   trim_char = "_")
#' if (inherits(ps, "phyloseq")) phyloseq::ntaxa(ps) else nrow(ps$otu_table)
#'
#' @export
abundance_taxid_to_phyloseq <- function(counts, metadata = NULL, xml = NULL,
                                       trim_char = FALSE) {
  parsed <- archi_read_abundance_taxid(counts)
  lin <- taxids_to_lineage(parsed$taxids, xml = xml)
  if (any(is.na(lin$taxid))) {
    missing <- parsed$taxids[is.na(lin$taxid)]
    stop("No lineage for taxid(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  ranks <- archi_rank_cols()
  rows <- list()
  k <- 0L
  for (j in seq_len(ncol(parsed$counts))) {
    for (i in seq_len(nrow(parsed$counts))) {
      reads <- parsed$counts[i, j]
      if (!is.finite(reads) || reads <= 0) next
      k <- k + 1L
      tip <- as.character(lin$tip_name[[i]])
      rows[[k]] <- data.frame(
        sample = colnames(parsed$counts)[[j]],
        taxid = parsed$taxids[[i]],
        name = if (is.na(tip) || !nzchar(tip)) paste0("tax_", parsed$taxids[[i]]) else tip,
        rank = "S",
        reads = reads,
        lin[i, ranks, drop = FALSE],
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) stop("Abundance table has no positive counts", call. = FALSE)
  legend <- NULL
  if (!is.null(metadata)) {
    if (is.character(metadata) && length(metadata) == 1L) {
      legend <- archi_read_sample_metadata(metadata)
      if (!isFALSE(trim_char)) {
        rownames(legend) <- vapply(strsplit(rownames(legend), trim_char, fixed = TRUE),
                                   function(z) z[[1]], character(1))
      }
    } else {
      legend <- as.data.frame(metadata, stringsAsFactors = FALSE)
    }
  }
  archi_assemble_phyloseq(do.call(rbind, rows), legend = legend)
}

archi_read_abundance_taxid <- function(counts) {
  if (is.character(counts) && length(counts) == 1L) {
    lines <- readLines(counts, warn = FALSE)
    sep <- if (any(grepl("\t", lines[[1]]))) "\t" else ","
    counts <- utils::read.delim(text = paste(lines, collapse = "\n"), sep = sep,
                                check.names = FALSE, stringsAsFactors = FALSE, quote = "")
  }
  if (is.data.frame(counts)) {
    id_col <- intersect(c("taxid", "taxonomy_id", "taxon_id", "TaxID", "tax_id"), names(counts))
    if (length(id_col)) {
      ids <- counts[[id_col[[1]]]]
      counts[[id_col[[1]]]] <- NULL
    } else {
      ids <- rownames(counts)
    }
    num <- vapply(counts, function(col) is.numeric(col) || !any(grepl("[A-Za-z]", col)), logical(1))
    counts <- as.matrix(data.frame(lapply(counts[num], as.numeric), check.names = FALSE))
    rownames(counts) <- as.character(ids)
  } else {
    counts <- as.matrix(counts)
  }
  storage.mode(counts) <- "double"
  taxids <- as.integer(rownames(counts))
  if (anyNA(taxids)) stop("Abundance row names must be NCBI taxids", call. = FALSE)
  list(counts = counts, taxids = taxids)
}

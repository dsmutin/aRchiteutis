# Taxonomy trees from the harness taxonomy-tree skill.
# Rank formula (ape::as.phylo), not an NCBI parent-id graph and not hclust.

archi_rank_cols <- function() {
  c("kingdom", "phylum", "class", "order", "family", "genus", "species")
}

archi_rank_map <- function() {
  c(
    domain = "kingdom", kingdom = "kingdom",
    phylum = "phylum", class = "class", order = "order",
    family = "family", genus = "genus", species = "species"
  )
}

archi_read_url_lines <- function(url) readLines(url, warn = FALSE)

archi_request_delay <- function(seconds) Sys.sleep(seconds)

#' Fill empty ranks with the last classified name
#'
#' Harness rule: an NA rank inherits the previous non-empty rank so
#' `ape::as.phylo` does not drop the tip.
#'
#' @param tax_df Data frame with Linnaean columns.
#' @param ranks Character vector of rank columns, coarse to fine.
#' @return The same data frame with gaps filled.
#' @export
fill_na_last_classified <- function(tax_df, ranks = archi_rank_cols()) {
  tax_df <- as.data.frame(tax_df, stringsAsFactors = FALSE)
  ranks <- intersect(ranks, names(tax_df))
  if (!length(ranks) || !nrow(tax_df)) return(tax_df)
  last <- rep(NA_character_, nrow(tax_df))
  for (rk in ranks) {
    current <- as.character(tax_df[[rk]])
    present <- !is.na(current) & nzchar(current)
    current[!present] <- last[!present]
    last[present] <- current[present]
    tax_df[[rk]] <- current
  }
  tax_df
}

archi_uniquify_last_rank <- function(vals, suffix) {
  vals <- as.character(vals)
  suffix <- as.character(suffix)
  dup <- duplicated(vals) | duplicated(vals, fromLast = TRUE)
  if (!any(dup)) return(vals)
  vals[dup] <- paste0(vals[dup], " [", suffix[dup], "]")
  vals
}

archi_sanitize_tax_df <- function(tax_df, cols) {
  keep <- as.data.frame(tax_df[, cols, drop = FALSE], stringsAsFactors = FALSE)
  as.data.frame(lapply(keep, function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x)] <- "Unclassified"
    x <- gsub("[[:punct:]]", "_", x)
    factor(x, levels = unique(x))
  }), stringsAsFactors = FALSE)
}

#' Build a taxonomy tree from a rank table
#'
#' Walks the seven Linnaean columns and keeps every rank as a node, including
#' unary parents. `ape::as.phylo` on a formula pastes those names into unquoted
#' Newick, so spaces in a species epithet split the tree and singleton ranks
#' collapse. Empty ranks still inherit the last classified name so a row always
#' has seven levels.
#'
#' @param lineage_df Data frame with one row per tip and columns among
#'   `kingdom`, `phylum`, `class`, `order`, `family`, `genus`, `species`.
#'   Optional `taxa_id`, `taxid`, and `tip_name`.
#' @return An [ape::phylo] object.
#'
#' @examples
#' lineage <- read.delim(
#'   system.file("extdata", "ncbi_lineage.tsv", package = "aRchiteutis"),
#'   stringsAsFactors = FALSE
#' )
#' tr <- ranks_to_tree(lineage)
#' tr$tip.label
#'
#' @export
ranks_to_tree <- function(lineage_df) {
  ranks <- archi_rank_cols()
  df <- as.data.frame(lineage_df, stringsAsFactors = FALSE)
  missing <- setdiff(ranks, names(df))
  for (rk in missing) df[[rk]] <- NA_character_
  if (!"tip_name" %in% names(df)) {
    df$tip_name <- ifelse(!is.na(df$species) & nzchar(as.character(df$species)),
                          as.character(df$species), as.character(df$genus))
  }
  if (!"taxa_id" %in% names(df)) {
    df$taxa_id <- if ("taxid" %in% names(df)) {
      paste0("tax_", df$taxid)
    } else {
      paste0("tip_", seq_len(nrow(df)))
    }
  }
  df <- fill_na_last_classified(df, ranks)
  last <- ranks[[length(ranks)]]
  last_vals <- as.character(df[[last]])
  empty <- is.na(last_vals) | !nzchar(last_vals)
  last_vals[empty] <- as.character(df$tip_name)[empty]
  suffix <- if ("taxid" %in% names(df)) df$taxid else df$taxa_id
  df[[last]] <- make.unique(archi_uniquify_last_rank(last_vals, suffix), sep = "_")
  sci <- as.character(df$tip_name)
  missing_sci <- is.na(sci) | !nzchar(sci)
  sci[missing_sci] <- as.character(df[[last]])[missing_sci]
  sci <- make.unique(sci, sep = "_")
  archi_phylo_from_rank_table(df[, ranks, drop = FALSE], sci)
}

#' ape phylo from rank columns, keeping unary nodes
#' @keywords internal
archi_phylo_from_rank_table <- function(rank_df, tip_labels) {
  rank_df <- as.data.frame(rank_df, stringsAsFactors = FALSE)
  n <- nrow(rank_df)
  if (n != length(tip_labels)) {
    stop("Rank table and tip labels must have the same length", call. = FALSE)
  }
  cols <- names(rank_df)
  paths <- lapply(seq_len(n), function(i) {
    vapply(cols, function(rk) {
      val <- as.character(rank_df[[rk]][i])
      if (is.na(val) || !nzchar(val)) val <- "Unclassified"
      paste(rk, val, sep = "=")
    }, character(1))
  })
  archi_phylo_from_rank_paths(paths, as.character(tip_labels))
}

#' Edge-list taxonomy tree from per-tip rank paths
#' @keywords internal
archi_phylo_from_rank_paths <- function(paths, tip_labels) {
  n_tip <- length(paths)
  if (n_tip < 2L) stop("Need at least two tips for a taxonomy tree", call. = FALSE)
  parent <- character()
  cum_paths <- vector("list", n_tip)
  for (i in seq_len(n_tip)) {
    parts <- paths[[i]]
    keys <- character(length(parts))
    acc <- ""
    for (j in seq_along(parts)) {
      acc <- if (j == 1L) parts[[j]] else paste(acc, parts[[j]], sep = "/")
      keys[[j]] <- acc
      par <- if (j == 1L) "" else keys[[j - 1L]]
      if (!acc %in% names(parent)) parent[acc] <- par
    }
    cum_paths[[i]] <- keys
  }
  tip_keys <- vapply(cum_paths, function(x) x[[length(x)]], character(1))
  if (anyDuplicated(tip_keys)) {
    dup <- duplicated(tip_keys)
    tip_keys[dup] <- paste0(tip_keys[dup], "#", seq_len(sum(dup)))
  }
  roots <- unique(names(parent)[parent == "" | is.na(parent)])
  if (length(roots) > 1L) {
    parent["__root__"] <- ""
    parent[roots] <- "__root__"
  }
  all_keys <- unique(c(names(parent), unlist(cum_paths, use.names = FALSE), tip_keys))
  internal_keys <- setdiff(all_keys, tip_keys)
  if (!length(internal_keys)) {
    parent[tip_keys] <- "__root__"
    parent["__root__"] <- ""
    internal_keys <- "__root__"
  }
  is_root <- vapply(internal_keys, function(k) {
    par <- unname(parent[k])
    !length(par) || is.na(par) || !nzchar(par)
  }, logical(1))
  internal_keys <- c(internal_keys[is_root], internal_keys[!is_root])
  id <- c(
    stats::setNames(seq_len(n_tip), tip_keys),
    stats::setNames(n_tip + seq_len(length(internal_keys)), internal_keys)
  )
  edge <- do.call(rbind, lapply(names(parent), function(child) {
    par <- unname(parent[[child]])
    if (!length(par) || is.na(par) || !nzchar(par)) return(NULL)
    c(unname(id[[par]]), unname(id[[child]]))
  }))
  storage.mode(edge) <- "integer"
  node_lab <- sub(".*=", "", internal_keys)
  node_lab[internal_keys == "__root__"] <- "root"
  structure(
    list(
      edge = edge,
      tip.label = as.character(tip_labels),
      Nnode = as.integer(length(internal_keys)),
      node.label = node_lab,
      edge.length = rep(1, nrow(edge))
    ),
    class = "phylo"
  )
}

#' Taxonomy tree from a rank table, or genus/species from labels
#' @param labels Character tip labels that match `tax$taxa` when `tax` is given.
#' @param tax Optional rank table.
#' @return An [ape::phylo] object.
#' @keywords internal
archi_taxa_tree <- function(labels, tax = NULL) {
  labels <- unique(as.character(labels))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (length(labels) < 2L) stop("Need at least two taxa for a taxonomy tree", call. = FALSE)
  if (is.null(tax)) return(archi_label_tree(labels))
  tax <- as.data.frame(tax, stringsAsFactors = FALSE)
  if (!"taxa" %in% names(tax)) tax$taxa <- rownames(tax)
  names(tax) <- tolower(names(tax))
  tax$taxa <- as.character(tax$taxa)
  tax <- tax[tax$taxa %in% labels, , drop = FALSE]
  ranks <- intersect(archi_rank_cols(), names(tax))
  if (length(ranks) >= 2L && nrow(tax) >= 2L) {
    tax$tip_name <- tax$taxa
    tr <- ranks_to_tree(tax)
    keep <- intersect(tr$tip.label, labels)
    if (length(keep) >= 2L) {
      return(ape::keep.tip(tr, keep, collapse.singles = FALSE))
    }
  }
  archi_label_tree(labels)
}

#' Parse an NCBI taxonomy efetch XML document
#'
#' @param xml Character. Document from
#'   `efetch.fcgi?db=taxonomy&retmode=xml`.
#' @return A lineage data frame, one row per requested taxid.
#' @export
parse_ncbi_taxonomy_xml <- function(xml) {
  xml <- paste(xml, collapse = "\n")
  parts <- regmatches(
    xml,
    gregexpr("(?s)<Taxon>.*?</LineageEx>", xml, perl = TRUE)
  )[[1]]
  if (!length(parts)) stop("No Taxon/LineageEx records in NCBI XML", call. = FALSE)
  ranks <- archi_rank_cols()
  map <- archi_rank_map()
  rows <- lapply(parts, function(chunk) {
    flat <- gsub("[\r\n]+", " ", chunk)
    taxid <- sub(".*?<TaxId>([0-9]+)</TaxId>.*", "\\1", flat)
    name <- sub(".*?<ScientificName>([^<]+)</ScientificName>.*", "\\1", flat)
    rank <- sub(".*?<Rank>([^<]+)</Rank>.*", "\\1", flat)
    lin <- sub(".*<LineageEx>(.*)</LineageEx>.*", "\\1", flat)
    nodes <- strsplit(lin, "<Taxon>", fixed = TRUE)[[1]]
    out <- stats::setNames(rep(NA_character_, length(ranks)), ranks)
    for (node in nodes) {
      nm <- sub(".*?<ScientificName>([^<]+)</ScientificName>.*", "\\1", node)
      rk <- sub(".*?<Rank>([^<]+)</Rank>.*", "\\1", node)
      if (!grepl("<ScientificName>", node, fixed = TRUE)) next
      if (!rk %in% names(map)) next
      dest <- map[[rk]]
      # Domain fills kingdom only until a real kingdom node appears.
      if (!is.null(dest) && (is.na(out[[dest]]) || identical(rk, dest))) {
        out[[dest]] <- nm
      }
    }
    dest_tip <- if (rank %in% names(map)) map[[rank]] else NULL
    if (!is.null(dest_tip)) out[[dest_tip]] <- name
    data.frame(
      taxid = as.integer(taxid),
      tip_name = name,
      tip_rank = rank,
      as.list(out),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Fetch NCBI lineages for a taxid vector and build a tree
#'
#' @param taxids Integer or character NCBI taxids.
#' @param xml Optional character XML. When supplied, no network call is made.
#'   The example reads a document fetched from NCBI and stored in the package.
#' @param batch_size Maximum taxids per NCBI efetch request.
#' @param delay Delay in seconds between live requests. The default stays
#'   below NCBI's unauthenticated limit of three requests per second.
#' @return For `taxids_to_lineage`, a data frame. For `taxids_to_tree`, a phylo.
#'
#' @examples
#' xml <- readLines(system.file("extdata", "ncbi_taxonomy.xml",
#'                              package = "aRchiteutis"), warn = FALSE)
#' lineage <- taxids_to_lineage(c(562, 1578), xml = paste(xml, collapse = "\n"))
#' tr <- taxids_to_tree(c(562, 1578), xml = paste(xml, collapse = "\n"))
#' sort(tr$tip.label)
#'
#' @export
taxids_to_lineage <- function(taxids, xml = NULL, batch_size = 100L, delay = 0.34) {
  taxids <- as.integer(taxids)
  taxids <- taxids[!is.na(taxids)]
  if (!length(taxids)) stop("No taxids", call. = FALSE)
  if (is.null(xml)) {
    ids <- unique(taxids)
    batch_size <- max(1L, as.integer(batch_size))
    batches <- split(ids, ceiling(seq_along(ids) / batch_size))
    records <- lapply(seq_along(batches), function(i) {
      if (i > 1L && is.finite(delay) && delay > 0) archi_request_delay(delay)
      url <- paste0(
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
        "?db=taxonomy&id=", paste(batches[[i]], collapse = ","),
        "&retmode=xml&tool=aRchiteutis"
      )
      parse_ncbi_taxonomy_xml(paste(archi_read_url_lines(url), collapse = "\n"))
    })
    lin <- do.call(rbind, records)
  } else {
    lin <- parse_ncbi_taxonomy_xml(xml)
  }
  lin[match(taxids, lin$taxid), , drop = FALSE]
}

#' @rdname taxids_to_lineage
#' @export
taxids_to_tree <- function(taxids, xml = NULL, batch_size = 100L, delay = 0.34) {
  ranks_to_tree(taxids_to_lineage(
    taxids, xml = xml, batch_size = batch_size, delay = delay
  ))
}

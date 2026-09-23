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
  for (i in seq_len(nrow(tax_df))) {
    last <- NA_character_
    for (rk in ranks) {
      v <- tax_df[[rk]][i]
      if (is.null(v) || length(v) == 0L || is.na(v) || !nzchar(as.character(v))) {
        tax_df[[rk]][i] <- if (!is.na(last) && nzchar(last)) last else NA_character_
      } else {
        last <- as.character(v)
      }
    }
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
#' Last formula term is the tip rank. Factors are built with
#' `as.character` then `factor` before `ape::as.phylo`, which is the harness
#' rule. Unary nodes collapse; multifurcations stay.
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
    df$tip_name <- ifelse(!is.na(df$species) & nzchar(df$species),
                          df$species, df$genus)
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
  df[[last]] <- archi_uniquify_last_rank(last_vals, suffix)
  tax_df_phy <- archi_sanitize_tax_df(df, ranks)
  form <- stats::as.formula(paste("~", paste(ranks, collapse = " / ")))
  tr <- ape::as.phylo(data = tax_df_phy, form)
  last_now <- as.character(df[[last]])
  sci <- as.character(df$tip_name)
  tip_map <- stats::setNames(sci, last_now)
  keep <- !is.na(tip_map) & nzchar(tip_map)
  keep <- keep & !(duplicated(names(tip_map)) | duplicated(names(tip_map), fromLast = TRUE))
  tip_map <- tip_map[keep]
  tr$tip.label <- ifelse(
    tr$tip.label %in% names(tip_map),
    unname(tip_map[tr$tip.label]),
    tr$tip.label
  )
  tr
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
taxids_to_lineage <- function(taxids, xml = NULL) {
  taxids <- as.integer(taxids)
  taxids <- taxids[!is.na(taxids)]
  if (!length(taxids)) stop("No taxids", call. = FALSE)
  if (is.null(xml)) {
    url <- paste0(
      "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy&id=",
      paste(taxids, collapse = ","),
      "&retmode=xml"
    )
    xml <- paste(readLines(url, warn = FALSE), collapse = "\n")
  }
  lin <- parse_ncbi_taxonomy_xml(xml)
  lin[match(taxids, lin$taxid), , drop = FALSE]
}

#' @rdname taxids_to_lineage
#' @export
taxids_to_tree <- function(taxids, xml = NULL) {
  ranks_to_tree(taxids_to_lineage(taxids, xml = xml))
}

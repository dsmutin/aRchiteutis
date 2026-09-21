#' Reshape a tidy count table into a taxa-by-sample matrix
#'
#' Pivots the long table produced by [get_counts()] into a numeric matrix with
#' taxa in the rows and samples in the columns, ready for base-graphics
#' heatmaps, clustering, PCA and other matrix-oriented plots.
#'
#' @param df A tidy `tibble` from [get_counts()].
#' @param clade Character or `FALSE`. Restrict to a single clade (e.g. `"G"`).
#' @param amount_from Character. Which value column to spread; one of
#'   `"amount"` (default), `"N"` or `"amount_cl"`.
#' @param top Integer or `FALSE`. Keep only the `top` most abundant taxa.
#' @param trim Integer or `FALSE`. Deprecated alias for `top`; used when `top`
#'   is `FALSE` so that legacy callers keep working.
#' @param scale `FALSE`, `"log2"` or `"scale"`. Optional per-column transform.
#' @param drop_unclassified Logical. Drop unclassified taxa first.
#' @param keep_sample_name Logical. If `FALSE`, column names encode the legend
#'   columns (joined with `"_"`) instead of the bare sample name.
#'
#' @return A numeric matrix (taxa in rows, samples in columns).
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m1[124]_", trim_char = "_")
#' mat <- df_untidy(df, clade = "G", top = 5)
#' dim(mat)
#'
#' @export
df_untidy <- function(df,
                      clade = FALSE,
                      amount_from = "amount",
                      top = FALSE,
                      trim = FALSE,
                      scale = FALSE,
                      drop_unclassified = FALSE,
                      keep_sample_name = TRUE) {

  # `trim` is a legacy alias for `top`
  if (isFALSE(top) && !isFALSE(trim)) top <- trim

  if (drop_unclassified) df <- df_tidy_drop_unclassified(df)
  if (!isFALSE(clade)) df <- df[df$clade %in% clade, ]

  if (!keep_sample_name && ncol(df) > 6) {
    df$sample <- apply(df[, c(7:ncol(df), 3)], 1,
                       function(z) stringr::str_c(z, collapse = "_"))
  }

  df <- df[, c("taxa", "sample", amount_from)]
  colnames(df)[3] <- "value"

  df <- dplyr::summarise(df, total = sum(value), .by = c("taxa", "sample"))

  res <- tidyr::pivot_wider(df, values_from = "total",
                            names_from = "sample", id_cols = "taxa")

  rn_res <- res$taxa
  res <- as.matrix(sapply(res[, -1, drop = FALSE], as.numeric))
  res[is.na(res)] <- 0
  rownames(res) <- rn_res

  if (!isFALSE(top)) {
    ord <- order(rowSums(res), decreasing = TRUE)
    res <- res[ord, , drop = FALSE]
    res <- res[seq_len(min(top, nrow(res))), , drop = FALSE]
  }

  if (!isFALSE(scale)) {
    rdf <- rownames(res)
    if (scale == "log2") res <- log2(res + 1)
    if (scale == "scale") res <- apply(res, 2, scale)
    rownames(res) <- rdf
  }

  res
}

#' Drop unclassified taxa
#'
#' @param df A tidy `tibble` from [get_counts()].
#' @return `df` with rows whose `taxa` contains `"unclassified"` removed.
#' @export
df_tidy_drop_unclassified <- function(df) {
  df[!stringr::str_detect(df$taxa, "unclassified"), ]
}

#' Get the filtered rows of the most abundant taxa
#'
#' Selects the `top` most abundant taxa (by mean `amount`) and returns the
#' subset of the long input table for those taxa. Note that this returns a
#' filtered long **data frame**, not a character vector, so it can be piped
#' straight into the composition plots.
#'
#' @param df A tidy `tibble` from [get_counts()].
#' @param drop_unclassified Logical. Drop unclassified taxa before ranking.
#' @param clade Character or `FALSE`. Restrict to a single clade.
#' @param top Integer. Number of taxa to keep.
#'
#' @return A filtered subset of `df` (same columns as the input).
#' @export
df_get_top_taxa <- function(df, drop_unclassified = TRUE, clade = FALSE, top = 50) {

  if (!isFALSE(clade)) df <- df[df$clade == clade, ]
  if (drop_unclassified) df <- df_tidy_drop_unclassified(df)

  taxa <- dplyr::summarise(df, m = mean(amount), .by = c("taxa", "clade"))
  taxa <- taxa[order(taxa$m, decreasing = TRUE), ]
  taxa <- taxa[seq_len(min(top, nrow(taxa))), c("taxa", "clade")]
  taxa <- as.character(apply(taxa, 1, function(z) stringr::str_c(z, collapse = "_")))

  df_taxa <- apply(df[, 1:2], 1, function(z) stringr::str_c(z, collapse = "_"))
  df[df_taxa %in% taxa, ]
}

#' Rescale sample amounts back to 1
#'
#' Recomputes the `amount` column so it sums to one within each sample. Useful
#' after taxa have been removed.
#'
#' @param df A tidy `tibble` from [get_counts()].
#' @return `df` with a rescaled `amount` column.
#' @export
df_rescale <- function(df) {
  df_sum <- dplyr::summarise(df, s = sum(N), .by = "sample")

  for (i in levels(df$sample)) {
    df[df$sample == i, 5] <- df[df$sample == i, 4] /
      unlist(df_sum[df_sum$sample == i, "s"])
  }

  message("rescaling done...")
  df
}

#' Drop redundant clade levels and level down unclassified reads
#'
#' Keeps only the canonical Kraken2 clades (`U`, `R`, `D`, `P`, `C`, `O`, `F`,
#' `G`, `S`) and re-labels unclassified reads one rank down.
#'
#' @param df A tidy `tibble` from [get_counts()].
#' @return A filtered `df` with cleaned `clade` values.
#' @export
df_drop_clade <- function(df) {
  clade_chr <- c("U", "R", "D", "P", "C", "O", "F", "G", "S")

  df <- df[df$clade %in% clade_chr, ]
  df$clade <- as.character(df$clade)

  df_uncl <- which(stringr::str_detect(df$taxa, "unclassified"))

  for (i in 9:1) {
    df_rename <- which(df$clade == clade_chr[i])
    df_rename <- df_rename[df_rename %in% df_uncl]
    df[df_rename, 2] <- c(clade_chr[-1], "remove")[i]
  }

  df <- df[!(df$clade == "remove"), ]

  message("few clade levels was dropped...")
  df
}

#' Add parent-taxa columns to the table
#'
#' Adds one column per clade level (`R`..`G`) holding the parent taxon at that
#' level for every row, propagating classification down the tree.
#'
#' @param df A tidy `tibble` from [get_counts()].
#' @return `df` with additional parent-taxa columns.
#' @export
df_get_parents <- function(df) {

  df <- df_drop_clade(df)
  res <- df
  df <- dplyr::summarise(df, .by = c("taxa", "clade"))

  clade_chr <- c("U", "R", "D", "P", "C", "O", "F", "G", "S")

  for (i in clade_chr[-c(1, 9)]) {
    df[, i] <- NA
    df_taxa <- which(df$clade == i)
    df_taxa <- c(df_taxa, length(df$clade) + 1)

    if (length(df_taxa) > 1) {
      for (j in seq_len(length(df_taxa) - 1)) {
        df[(df_taxa[j] + 1):(df_taxa[j + 1] - 1), i] <- df$taxa[df_taxa[j]]
      }
    }
  }

  df <- df[!is.na(df$taxa), ]

  for (i in 2:7) {
    df[df$clade == clade_chr[i], clade_chr[(i + 1):8]] <- NA
  }

  df_uncl <- which(stringr::str_detect(df$taxa, "unclassified"))

  for (i in df_uncl) {
    df_uncl_N <- stringr::str_remove(
      as.character(unlist(df[i, 1])), " unclassified")
    df_uncl_leg <- which(df[, 1] == df_uncl_N)
    df[i, clade_chr[2:8]] <- df[df_uncl_leg[1], clade_chr[2:8]]

    df_uncl_na <- is.na(df[i, clade_chr[2:8]])
    if (sum(df_uncl_na) > 0) {
      df[i, clade_chr[which(df_uncl_na)[1] + 1]] <-
        stringr::str_remove(as.character(unlist(df[i, 1])), " unclassified")
    }
  }

  res <- dplyr::left_join(res, df, by = c("taxa", "clade"))

  message("classification done")
  res
}

#' Keep only the top taxa (and roll the rest into an "other" group)
#'
#' Selects the most abundant taxa across the tree, keeping at most `top_taxa`
#' distinct lineages, and rolls remaining reads of each domain into an
#' `"other <domain>"` row.
#'
#' @param df A tidy `tibble` from [get_counts()].
#' @param top_taxa Integer. Number of taxa to keep.
#' @return A filtered `df` with `"other"` rows added.
#' @export
df_taxa_trim <- function(df, top_taxa = 10) {
  clade_chr <- c("U", "R", "D", "P", "C", "O", "F", "G", "S")

  df2 <- df_get_parents(df)

  df2 <- as.data.frame(dplyr::summarise(
    df2, m = mean(amount), .by = c("taxa", "clade", clade_chr[2:8])))
  df2 <- df2[order(df2$m, decreasing = TRUE), setdiff(names(df2), "m")]
  df2 <- as.data.frame(apply(df2, 2, as.character))

  res <- df2[1, ]

  i <- 1
  while (nrow(res) < (top_taxa - 1) && i <= nrow(df2)) {
    res_line <- df2[i, ]
    df_parents <- levels(droplevels(as.factor(
      unlist(res[, clade_chr[2:8]]))))

    if (!(res_line$taxa %in% df_parents)) {
      res <- res[!(res$taxa %in% res_line[, clade_chr[2:8]]), ]
      res <- rbind(res, res_line)
    }
    i <- i + 1
  }

  df_other <- df[df$clade == "D", ]
  df_other <- df_other[!stringr::str_detect(df_other$taxa, " unclassified"), ]
  df_other$taxa <- stringr::str_c("other ", df_other$taxa)

  df_res <- df[df$taxa %in% res$taxa, ]
  df_sum <- dplyr::summarise(
    df_res,
    N = sum(N), amount = sum(amount), amount_cl = sum(amount_cl),
    .by = "sample")

  df_other <- dplyr::left_join(df_other, df_sum, by = "sample",
                               suffix = c("", ".tot"))
  df_other$N <- df_other$N - df_other$N.tot
  df_other$amount <- df_other$amount - df_other$amount.tot
  df_other$amount_cl <- df_other$amount_cl - df_other$amount_cl.tot
  df_other <- df_other[, !grepl("\\.tot$", names(df_other))]

  df_res <- rbind(df_res, df[df$taxa == "unclassified", ], df_other)

  message("taxa trimmed...")
  df_res
}

#' Remove specific taxa from the table
#'
#' @param df A tidy `tibble` from [get_counts()].
#' @param taxa Character vector of taxa names to remove.
#' @param clade Character or `FALSE`. If set, only remove matching taxa within
#'   this clade.
#' @return `df` with the requested taxa removed.
#' @export
df_remove_taxa <- function(df, taxa, clade = FALSE) {
  drop <- df$taxa %in% taxa
  if (!isFALSE(clade)) drop <- drop & (df$clade %in% clade)
  df[!drop, ]
}

#' Bind several count tables, recomputing unclassified amounts
#'
#' Row-binds count tables and recomputes the unclassified fraction using the
#' totals from the first table.
#'
#' @param f1 First tidy `tibble` from [get_counts()]; used as the reference for
#'   unclassified amounts.
#' @param ... Further tidy tables to bind.
#' @return A combined `tibble`.
#' @export
df_smart_bind <- function(f1, ...) {
  message("Unclassified amount parsed by 1st file")

  df <- rbind(f1, ...)

  df_sum <- dplyr::summarise(
    subset(f1, clade %in% c("U", "R")), s = sum(N), .by = "sample")
  colnames(df_sum) <- c("sample", "sum")

  df_cl_sum <- dplyr::summarise(
    subset(df, clade == "R"), cls = sum(N), .by = "sample")

  df_sum <- dplyr::left_join(df_sum, df_cl_sum, by = "sample")

  uncl <- f1[f1$clade == "U", ]
  df_uncl <- data.frame(sample = df_sum$sample,
                        sum = df_sum$sum - df_sum$cls,
                        am = (df_sum$sum - df_sum$cls) / df_sum$sum)
  uncl <- dplyr::left_join(uncl, df_uncl, by = "sample")
  uncl$N <- uncl$sum
  uncl$amount <- uncl$am
  uncl$amount_cl <- 0

  df <- dplyr::left_join(df[df$clade != "U", ], df_sum, by = "sample")
  df$amount <- df$N / df$sum
  df$amount_cl <- df$N / df$cls

  df <- dplyr::bind_rows(df, uncl)
  df[, !(names(df) %in% c("sum", "cls", "am"))]
}

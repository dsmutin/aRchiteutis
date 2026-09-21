#' Read Kraken2 / Kaiju / Bracken reports into a tidy count table
#'
#' Reads every classifier report in a directory and assembles a single long
#' (tidy) `tibble` with one row per taxon per sample. An optional legend file
#' can be joined onto the samples.
#'
#' @param path Character. Path to the directory holding the report files.
#' @param keep_unclassified Logical. If `TRUE` (default), reads counted as
#'   unclassified below a rank are kept as new `"<taxon> unclassified"` rows.
#' @param pattern Character. Optional regular expression used to select which
#'   files in `path` to read.
#' @param trim_char Character or `FALSE`. Optional delimiter used to split each
#'   file name; the first field becomes the sample name.
#' @param output_type Character. Type of classifier output, one of
#'   `"kraken2"` (default), `"kaiju"` or `"bracken"`.
#' @param legend Character or `FALSE`. Optional path to a CSV legend whose first
#'   column matches the (possibly trimmed) sample name; remaining columns are
#'   joined onto the table.
#'
#' @return A [tibble::tibble] with columns `taxa`, `clade`, `sample`, `N`
#'   (number of reads), `amount` (fraction of all reads), `amount_cl` (fraction
#'   of classified reads) and any additional legend columns.
#'
#' @examples
#' path <- system.file("extdata", package = "aRchiteutis")
#' legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m1[12]_", legend = legend,
#'                  trim_char = "_")
#' head(df)
#'
#' @export
get_counts <- function(path,
                       keep_unclassified = TRUE,
                       pattern = "",
                       trim_char = FALSE,
                       output_type = "kraken2",
                       legend = FALSE) {

  file_list <- dir(path, pattern = pattern)
  res <- data.frame()

  # kraken2 ----
  if (output_type == "kraken2") {
    for (file in file_list) {
      df <- utils::read.table(paste0(path, "/", file), sep = "\t",
                              fill = TRUE, quote = "")

      colnames(df) <- c("amount", "N", "NUnCl", "clade", "id", "taxa")

      df$taxa <- stringr::str_trim(df$taxa, "both")
      row.names(df) <- df$id

      df[, 2:3] <- sapply(df[, 2:3], as.numeric)
      df[, 4] <- as.factor(df[, 4])
      df[, 6] <- as.factor(df[, 6])

      # make unclassified ranks their own taxa
      if (isTRUE(keep_unclassified)) {
        df_uncl <- which(df$NUnCl != 0)
        df_uncl <- df[df_uncl, ]
        df_uncl$N <- df_uncl$NUnCl
        df_uncl$taxa <- stringr::str_c(df_uncl$taxa, " unclassified")
        rownames(df_uncl) <- stringr::str_c(rownames(df_uncl), "_1")
        df <- rbind(df, df_uncl[-1, ])
      }

      # drop NUnCl and id, then rescale
      df <- df[, -c(3, 5)]
      df_sum <- sum(df[c("0", "1"), 2])
      df$amount <- df$N / df_sum
      df$amount_cl <- df$N / df[c("1"), 2]

      # add sample and trim file name
      if (!isFALSE(trim_char)) {
        file <- stringr::str_split(file, trim_char)[[1]][1]
      }
      df$sample <- file
      df <- df[, c(4, 3, 6, 2, 1, 5)]

      res <- rbind(res, df)
      message(file, " done")
    }
  }

  # kaiju ----
  if (output_type == "kaiju") {
    for (file in file_list) {
      df <- utils::read.table(paste0(path, "/", file), sep = "\t", header = FALSE)

      df_names <- stringr::str_split(df$V4, "; ")
      df_names <- as.data.frame(do.call(rbind, df_names))

      ldfn <- length(df_names[1, ])
      main <- df_names[1, 1]
      df_taxa_vector <- c()
      df_genus_vector <- c()

      for (i in seq_len(nrow(df_names))) {
        if (sum(df_names[i, -1] %in% c("", main)) > 0) {
          df_clear <- which(df_names[i, -1] %in% c("", main)) + 1
          df_names[i, df_clear[1]:ldfn] <- NA
        }
        df_taxa <- which(is.na(df_names[i, ]))
        df_taxa_vector <- c(df_taxa_vector, df_names[i, df_taxa[1] - 1])

        df_genus <- which(
          (stringr::str_detect(df_names[i, ], " ")) & !(
            (stringr::str_detect(df_names[i, ], "incertae sedis")) |
              (stringr::str_detect(df_names[i, ], "species")) |
              (stringr::str_detect(df_names[i, ], "cellular"))))

        df_genus <- stringr::str_split(df_names[i, max(df_genus)], " ")[[1]][1]
        if (is.null(df_genus)) df_genus <- paste0("unclassified ", df_names[i, 6])

        df_genus_vector <- c(df_genus_vector, df_genus)
      }
      df_genus_vector[df_genus_vector == "unclassified NA"] <- "unclassified"

      message("names from ", file, " parsed...")

      df <- tibble::tibble(taxa = df_genus_vector,
                           clade = "G",
                           sample = file,
                           N = 1)

      df <- dplyr::summarise(df, N = sum(N),
                             .by = c("taxa", "clade", "sample"))
      df$amount <- df$N / sum(df$N)
      df$amount_cl <- df$amount

      res <- rbind(res, df)
    }

    if (!isFALSE(trim_char)) {
      res$sample <- vapply(stringr::str_split(res$sample, trim_char),
                           function(z) z[1], character(1))
    }
  }

  # bracken ----
  if (output_type == "bracken") {
    for (fname in file_list) {
      df <- utils::read.table(paste0(path, "/", fname), sep = "\t", header = TRUE)

      if (!isFALSE(trim_char)) {
        fname <- stringr::str_split(fname, trim_char)[[1]][1]
      }

      df <- data.frame(taxa = df$name,
                       clade = df$taxonomy_lvl,
                       sample = fname,
                       N = df$new_est_reads,
                       amount = df$fraction_total_reads)
      df$amount_cl <- df$N / sum(df$N)

      res <- rbind(res, df)
      message(fname, " done")
    }
  }

  # add legend ----
  if (!isFALSE(legend)) {
    legend <- utils::read.csv(legend, header = TRUE, row.names = 1,
                              stringsAsFactors = TRUE)

    if (!isFALSE(trim_char)) {
      legend$sample <- vapply(stringr::str_split(row.names(legend), trim_char),
                              function(z) z[1], character(1))
    } else {
      legend$sample <- row.names(legend)
    }

    res <- dplyr::left_join(res, legend, by = "sample")
    message("Legend applied")
  }

  res$sample <- as.factor(res$sample)
  message("all files added to table...")
  tibble::as_tibble(res)
}

#' Extract taxonomy strings from Kraken2 reports
#'
#' Utility that scrapes the clade/taxon columns from a set of Kraken2 reports,
#' optionally restricting the output to the sub-trees rooted at a set of taxa of
#' a given clade.
#'
#' @param path Character. Directory holding the report files.
#' @param pattern Character. Optional regular expression selecting files.
#' @param extract_taxa Character vector or `FALSE`. Taxa (all of the same clade)
#'   whose sub-trees should be extracted.
#' @param extract_clade Character. Classification level of `extract_taxa`
#'   (e.g. `"G"`).
#'
#' @return A `data.frame` with unique `clade` / `taxa` rows.
#' @export
get_kraken_taxonomy <- function(path,
                                pattern = "",
                                extract_taxa = FALSE,
                                extract_clade = NULL) {
  file_list <- dir(path, pattern = pattern)
  res <- data.frame()

  for (file in file_list) {
    df <- utils::read.table(paste0(path, "/", file), sep = "\t",
                            fill = TRUE, quote = "")

    colnames(df)[1:6] <- c("amount", "N", "NUnCl", "clade", "id", "taxa")

    df <- df[, c("clade", "taxa")]
    df$taxa <- stringr::str_remove(df$taxa, "^ *")

    if (!isFALSE(extract_taxa)) {
      c_list <- which(df$clade == extract_clade)
      t_list <- which(df$taxa %in% extract_taxa)
      t_list <- t_list[t_list %in% c_list]
      tcint <- c_list %in% t_list

      if (tcint[length(tcint)]) {
        tcint <- c(tcint, FALSE)
        c_list <- c(c_list, nrow(df) + 1)
      }

      c_list[!tcint] <- c_list[!tcint] - 1

      keep_df <- data.frame(n1 = c_list[tcint],
                            n2 = c_list[c(FALSE, tcint[-length(tcint)])])

      keep_num <- c()
      for (i in seq_len(nrow(keep_df))) {
        keep_num <- c(keep_num, keep_df$n1[i]:keep_df$n2[i])
      }

      df <- df[keep_num, ]
    }

    res <- rbind(res, df)
    message(file, " done")
  }

  unique(res)
}

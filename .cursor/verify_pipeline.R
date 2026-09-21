#!/usr/bin/env Rscript
## Smoke test / demonstration for the aRchiteutis / samovar environment.
## Loads the bundled example Kraken2 reports, runs the data-manipulation
## pipeline, and renders every visualization to PNG files.
##
## Usage:  Rscript .cursor/verify_pipeline.R [output_dir]
## Exits non-zero if any step fails.

repo_root <- tryCatch(
  dirname(dirname(normalizePath(sub("^--file=", "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])))),
  error = function(e) getwd())
if (is.na(repo_root) || !file.exists(file.path(repo_root, "source_functions.R"))) {
  repo_root <- getwd()
}
setwd(repo_root)

args <- commandArgs(trailingOnly = TRUE)
outdir <- if (length(args) >= 1) args[[1]] else file.path(tempdir(), "samovar_plots")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(source("source_functions.R"))
suppressPackageStartupMessages({
  library(abdiv); library(usedist); library(heatmap3); library(ape)
  library(ggnewscale); library(ggforce); library(tsne)
})

ok <- character(0); fail <- character(0)
step <- function(name, expr) {
  res <- tryCatch({ force(expr); TRUE },
                  error = function(e) { cat("  [FAIL]", name, "-", conditionMessage(e), "\n"); FALSE })
  if (isTRUE(res)) { cat("  [ OK ]", name, "\n"); ok[[length(ok) + 1]] <<- name }
  else fail[[length(fail) + 1]] <<- name
}
gg   <- function(f, p, w = 8, h = 6) ggsave(file.path(outdir, f), p, width = w, height = h, dpi = 110)
base <- function(f, expr, w = 900, h = 700) { png(file.path(outdir, f), width = w, height = h); on.exit(dev.off()); force(expr) }

## ---- Load example data ------------------------------------------------------
df_full <- get_counts(path = "test/example", pattern = "decont_b",
                      legend = "test/example/legend.csv", trim_char = "_")
df_full$sample <- df_full$sample %>% str_remove("m") %>% fct_inseq
df_full <- df_full[order(df_full$sample), ]
df <- df_full[df_full$clade != "S", ]
cat(sprintf("Loaded %d rows, %d samples, %d genera, %s classified reads\n",
            nrow(df), length(unique(df$sample)),
            length(unique(droplevels(df$taxa[df$clade == "G"]))),
            format(sum(unlist(summarise(df[df$clade == "R", ], sum(N)))), big.mark = ",")))

dfT <- df %>% df_taxa_trim(top_taxa = 18) %>% df_tidy_drop_unclassified() %>%
  subset(amount > 0) %>% df_rescale
dfC15 <- df %>% df_get_top_taxa(clade = "G", top = 15)

## ---- Render every plot ------------------------------------------------------
step("df2donut",        gg("donut.png", df2donut(dfT)))
step("df2composition",  gg("composition.png", df2composition(dfT)))
step("df2barplot",      gg("barplot.png", df2barplot(dfC15)))
step("df2alpha_summary",gg("alpha.png", df2alpha_summary(subset(df_full, clade == "S"), split_by = 7, add_legend = 7:8), 11, 8))
step("df2beta",         base("beta.png", df2beta(df, add_legend = 7:8, treshhold_down = 1e-5)))
step("df2pca_sample",   gg("pca_sample.png", df2pca_sample(df_untidy(df_full, clade = "S", scale = "scale", keep_sample_name = FALSE), scale = FALSE, detect = "pupa", geom.ind = "point")))
step("df2pca_sp",       gg("pca_sp.png", df2pca_sp(df_untidy(df, clade = "G", top = 10), scale = TRUE)))
step("df2heatmap",      base("heatmap.png", df2heatmap(df_untidy(df, clade = "G", top = 10), scale = "row", Colv = NA)))
step("df2cluster",      base("cluster.png", df2cluster(df_untidy(df, clade = "C", top = 30, scale = "log2"), k_means = 10, use = "sp")))
step("df2corrplot",     base("corrplot.png", df2corrplot(df_untidy(df_tidy_drop_unclassified(df), clade = "G", top = 30, scale = "scale"), k_means = 5)))
step("df2chord",        gg("chord.png", df2chord(df_untidy(df, clade = "G", drop_unclassified = TRUE, top = 50, scale = "scale"), k_means = 10, coenf_level = 0.7), 10, 10))
step("df2tsne",         gg("tsne.png", df2tsne(df_untidy(df, clade = "G", drop_unclassified = TRUE, scale = "scale"), k_means = 20, text_top = 20)))
step("df2volcano",      gg("volcano.png", df2volcano(subset(df, clade == "G"), legend_detect = c("pupa", "larvae"), treshhold_logAC = 0.3)))

cat(sprintf("\nPASSED %d / %d  (plots in %s)\n", length(ok), length(ok) + length(fail), outdir))
if (length(fail) > 0) { cat("FAILED:", paste(fail, collapse = ", "), "\n"); quit(status = 1) }
cat("All pipeline steps passed.\n")

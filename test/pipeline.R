# Full walkthrough of the aRchiteutis plotting functions on the bundled data.
# (This mirrors the historical test/pipeline.R but targets the installed
# package API.)

library(aRchiteutis)
library(ggplot2)

# preparing file ----
path <- system.file("extdata", package = "aRchiteutis")
legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")

df <- get_counts(path = path, pattern = "decont_b", legend = legend,
                 trim_char = "_")

df$sample <- forcats::fct_inseq(stringr::str_remove(df$sample, "m"))
df <- df[order(df$sample), ]

# Keep a copy with species retained: alpha-diversity and sample-level PCA need
# species-level rows.
df_all <- df

# if you don't totally trust Kraken, drop all species levels for the rest
df <- df[df$clade != "S", ]

# calculate number of reads, species and genus ----
sum(df$N[df$clade == "R"])                 # number of classified reads
length(unique(df$taxa[df$clade == "G"]))   # number of genera

# trim several taxa or choose a clade to use ----
dfT <- df_taxa_trim(df, top_taxa = 18) |>
  df_tidy_drop_unclassified() |>
  subset(amount > 0) |>
  df_rescale()

dfO <- df_tidy_drop_unclassified(df) |>
  df_taxa_trim(top_taxa = 15) |>
  subset(!stringr::str_detect(taxa, "other")) |>
  df_rescale()

dfC <- df_get_top_taxa(df, clade = "G", top = 50)

# diversity plots ----
df2alpha_summary(subset(df_all, clade == "S"), split_by = 7, add_legend = 7:8)

# Bray-Curtis beta diversity (operates on the long table)
df2beta_bray(df, clade = "G", add_legend = 7:8)

df2beta(df, add_legend = 7:8, treshhold_down = 10^(-5))

# composition plots ----
df2donut(dfT)
subset(dfT, taxa != "unclassified") |> df2donut(color = "white")

df2composition(dfT)
df2composition(dfO)
df2composition(dfC)

df2barplot(dfC) + coord_trans(x = "log")

# base R heatmap (wide matrix input) ----
df_untidy(df, clade = "G", top = 10) |>
  df2heatmap(scale = "row", Colv = NA)

# base cluster for samples
df_untidy(df, clade = "C", top = 30, scale = "log2") |>
  df2cluster(k_means = 10, use = "sp")

# PCA for samples (species retained)
df_untidy(df_all, clade = "S", scale = "scale", keep_sample_name = FALSE) |>
  df2pca_sample(scale = FALSE, detect = "pupa", geom.ind = "point")

# species-level plots ----
df_untidy(df, clade = "G", scale = "scale") |>
  df2cluster(k_means = 0, use = "sample")

subset(df, clade == "G") |>
  df2clust2d(top = 40, k_means = 10, legend_detect = "pupa")

df_untidy(df, clade = "G", drop_unclassified = TRUE, scale = "scale") |>
  df2tsne(k_means = 20, text_top = 20)

subset(df, clade == "G") |>
  df2volcano(legend_detect = c("pupa", "larvae"), treshhold_logAC = 0.3)

# correlations (the legacy `trim` alias for `top` still works)
df_tidy_drop_unclassified(df) |>
  df_untidy(clade = "G", trim = 30, scale = "scale") |>
  df2corrplot(k_means = 5)

# chord
df_untidy(df, clade = "G", drop_unclassified = TRUE, top = 50, scale = "scale") |>
  df2chord(k_means = 10, coenf_level = 0.7)

# PCA for taxa
df_untidy(df, clade = "G", top = 10) |>
  df2pca_sp(scale = TRUE)

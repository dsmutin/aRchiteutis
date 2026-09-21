# Example pipeline for the aRchiteutis package.
#
# Install once with:  remotes::install_github("dsmutin/aRchiteutis")
# then simply attach it:

library(aRchiteutis)
library(ggplot2)

# preparing file ----
# The reports shipped with the package live in inst/extdata; point `path` at
# your own directory of Kraken2 reports instead.
path <- system.file("extdata", package = "aRchiteutis")
legend <- system.file("extdata", "legend.csv", package = "aRchiteutis")

df <- get_counts(
  path = path,
  pattern = "decont_b",
  legend = legend,
  trim_char = "_")

# if you don't totally trust Kraken, drop all species levels
df_nosp <- df[df$clade != "S", ]

# trim several taxa or choose a clade to use ----
dfT <- df_taxa_trim(df_nosp, top_taxa = 15)
dfC <- df_get_top_taxa(df, clade = "G", top = 50)

# composition plots ----
df2donut(dfT)
df2composition(dfT)
df2barplot(dfC)

# diversity ----
df2alpha_summary(df[df$clade == "S", ], split_by = 7, add_legend = 7:8)
df2beta(df, clade = "G", add_legend = 7:8)
df2beta_pcoa(df[df$clade == "G", ], add_legend = 7, add_ellipse = 7)

# ordination / clustering (wide matrix input) ----
matG <- df_untidy(df, clade = "G", top = 30, scale = "scale")
df2heatmap(df_untidy(df, clade = "G", top = 10), scale = "row", Colv = NA)
df2cluster(matG, k_means = 10, use = "sp")
df2clust2d(df[df$clade == "G", ], legend_detect = "pupa", top = 40, k_means = 10)
df2corrplot(matG, k_means = 5)
df2chord(matG, k_means = 10, coenf_level = 0.7)
df2tsne(matG, k_means = 10, text_top = 20)
df2volcano(df[df$clade == "G", ], legend_detect = c("pupa", "larvae"))

# PCA ----
df2pca_sample(df_untidy(df, clade = "S", scale = "scale",
                        keep_sample_name = FALSE),
              scale = FALSE, detect = "pupa")
df2pca_sp(df_untidy(df, clade = "G", top = 10), scale = TRUE)

# python2r: abundance table loaded in Python, plotted in R ----
# In Python:  python3 inst/python/python2r.py my_table.csv abundance.csv
# (or `from python2r import export_abundance` in a notebook)
py_csv <- system.file("extdata", "python_abundance.csv", package = "aRchiteutis")
df_py <- python2r(py_csv, clade = "G")
df2composition(df_py)
df2barplot(df_py)

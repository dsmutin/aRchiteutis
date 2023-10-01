#source functions and set working directory ----

setwd("~/R/aRchiteutis/")
source("source_functions.R")


#preparing file ----

df <- get_counts(
  path = "PATH/TO/KRAKEN/REPORTS",
  pattern = "SUBSET_CHARACTERS",
  legend = "PATH/TO/LEGEND",
  trim_char = "CHARACTER_TO_SPLIT_FILENAMES")


#calculate number of reads, species and genus
summarise(df[df$clade == "R",], sum(N)) %>% unlist #number of classified reads
df$taxa[df$clade == "S"] %>% droplevels %>% levels %>% length #number of species
df$taxa[df$clade == "G"] %>% droplevels %>% levels %>% length #number of genus

#if you don't totally trust Kraken, drop all species levels
df <- df[df$clade != "S",]

#trim several taxa or chose clade to use (examples)
dfT <- df %>% df_taxa_trim(top_taxa = 12) #get any top taxa

dfO <- df %>% df_tidy_drop_unclassified %>% df_taxa_trim(top_taxa = 0) 
dfO <- dfO[dfO$taxa != "other Bacteria",] %>% df_rescale #exclude "other" levels

dfC <- df %>% df_tidy_drop_unclassified %>% df_get_top_taxa("G", 50)
dfC <- df[df$taxa %in% dfC,] #get top genus taxa

#donuts
df2donut(dfT)
df2donut(dfT[dfT$taxa != "unclassified",], show.legend = F, color = "white")  +
  theme(text = element_text(size = 30),
        legend.position = "bottom")

#profiles
df2composition(dfT)
df2composition(dfO)
df2composition(dfC)

#barplots
df2barplot(dfO) + coord_trans(x = "log") + theme(text = element_text(size = 20))

#base R heatmap
df2heatmap(df, "G", trim = 10,
           scale = "column", Colv = NA)

#base cluster
df2cluster(dfC, k_means = 10)

#2D cluster by legend variables
df2clust2d(dfC, k_means = 10, legend_detect = "pupa")

#correlations
df2corrplot(dfO, log2_scale = T)

#chord. for big amount of data strongly recommend save plots without observing
df2chord(dfC, k_means = 10)

#tSNE for taxa
taxa <- df %>% df_tidy_drop_unclassified %>% df_get_top_taxa("S", 20)
df2tsne(df, clade_trim = "S", counts = F, log2df = F, taxa = taxa)


#PCA
df2pca_sample(df, "S", scale = T)
df2pca_sp(dfO, scale = T)

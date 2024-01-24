#source functions and set working directory ----
#warning! do not attach plyr library
setwd("~/bioinformatics/R/aRchiteutis/")
source("source_functions.R")

#preparing file ----
df <- get_counts(
  path = "./example/",
  pattern = "decont_b",
  legend = "./example/legend.csv",
  trim_char = "_")

df$sample <- df$sample %>% str_remove("m") %>% fct_inseq
df <- df[order(df$sample),]

#if you don't totally trust Kraken, drop all species levels
df <- df[df$clade != "S",]


#calculate number of reads, species and genus ----
summarise(df[df$clade == "R",], sum(N)) %>% unlist #number of classified reads
df$taxa[df$clade == "S"] %>% droplevels %>% levels %>% length #number of species
df$taxa[df$clade == "G"] %>% droplevels %>% levels %>% length #number of genus


#trim several taxa or chose clade to use (examples) ----
dfT <- df %>% df_taxa_trim(top_taxa = 15) #get any top taxa

dfT <- df %>% 
  df_taxa_trim(top_taxa = 18) %>% 
  df_tidy_drop_unclassified() %>%
  subset(amount > 0) %>%
  df_rescale

dfO <- df %>% 
  df_tidy_drop_unclassified %>% 
  df_taxa_trim(top_taxa = 15) %>%
  subset(!(taxa %>% str_detect("other"))) %>% 
  df_rescale #exclude "other" levels

dfC <- df %>% df_get_top_taxa(clade = "G", top = 50) #get top genus taxa

#diversity plots ----
df %>% subset(clade == "S") %>%
  df2alpha_summary(split_by = 7, add_legend = 7:8)

df %>% df_untidy(drop_unclassified = T, keep_sample_name = F, clade = "G") %>%
  df2beta_bray()

df %>%
  df2beta(add_legend = 7:8, 
          treshhold_down = 10^(-5)
          )


#composition plots ----
#donuts
df2donut(dfT)

dfT[dfT$taxa != "unclassified",] %>% 
  df2donut(show.legend = F, color = "white")

#profiles
df2composition(dfT)
df2composition(dfO)
df2composition(dfC)

#barplots
df2barplot(dfC) + coord_trans(x = "log") + 
  theme(axis.text.y = element_text(face = "italic"),
  text = element_text(size = 20))

#base R heatmap #wider tables required!
df %>% df_untidy (clade = "G", top = 10) %>% 
  df2heatmap(scale = "row", Colv = NA)

#base cluster for samples #wider tables required!
df %>% df_untidy (clade = "C", top = 30, scale = "log2") %>% 
  df2cluster(k_means = 10, use = "sp")

#PCA for samples
df %>% df_untidy(clade = "S", scale = "scale", keep_sample_name = F) %>%
  df2pca_sample(scale = F, detect = "pupa", geom.ind = "point")

# species plots ----

#base cluster for species #wider tables required!
df %>% df_untidy (clade = "G", scale = "scale") %>% 
  df2cluster(k_means = 0, use = "sample")

#2D cluster by legend variables
df %>% subset(clade == "G") %>%
  df2clust2d(top = 40, k_means = 10, legend_detect = c("pupa", "larvae"))

#tSNE for taxa #wider tables required!
df %>% df_untidy (clade = "G", drop_unclassified = T, scale = "scale") %>% 
  df2tsne(k_means = 20, text_top = 20)

#volcano plot
df %>% subset(clade == "G") %>%
  df2volcano(legend_detect = c("pupa", "larvae"), treshhold_logAC = 0.3)
  
#correlations #wider tables required!
df %>% df_tidy_drop_unclassified %>% df_untidy (clade = "G", trim = 30, scale = "scale") %>% 
  df2corrplot(k_means = 5)

#chord  #wider tables required!
#for big amount of taxa strongly recommend save plots without observing. example
#ggsave(filename = "~/chord.png", scale = 1.2, width = 2000, height = 2000, units = "px")
df %>% df_untidy(clade = "G", drop_unclassified = T, top = 50, scale = "scale") %>%
  df2chord(k_means = 10, coenf_level = 0.7)

#PCA
df %>% df_untidy(clade = "G", top = 10) %>%
  df2pca_sp(scale = T)

# aRchiteutis: tool for visualize &amp; work with Kraken2 reports

To run analysis you only need to work with file "pipeline.R"

# Functions

All functions for data preparation and their descriptions are avialable in "functions"
All manipulations to visualize data is avialable in "plots"

Enjoy beauty of R plots!

![изображение](https://github.com/dsmutin/aRchiteutis/assets/98605724/6502f374-a271-470c-af4e-30b8ff531ba3)


# Pipeline itself with description

get_counts: get conut and amount table from k2 report

      path, #path to k2 reports
      keep_unclassified = T, #keep or not unclassified reads from report as new taxa
      pattern = "", #optional: pattern of file names to use
      trim_char = F, #optional: character to split file name in final table
      clade = F, #optional: which clade (like "O", "F", "G", "S" to use)
      type = k2, #type, now only available for kraken2
      legend = F #optional: path to legend file in csv format: 
                 #legend$name == name of kraken report (OR slited file name!), rest of columns - legend itself
  
  result is data.frame with columns: 
  
          #taxa, 
          #clade, 
          #sample, 
          #number of reads, 
          #ammount of all reads, 
          #amount of classified reads 
          #and several columns of applied legend


df_untidy: get untidy table for base::heatmap or future processing

      df, #result tibble from get_counts.R
      counts = T, #use counts or amount data
      classified_only = F, #only if counts = F, get amount by classified data
      drop_unclassified = F, #drop all unclasified levels
      keep_sample_name = F #warning! if T translate legend to separated format in name of each column :(


df_taxa_trim: trim taxa by their amount

      df, #result tibble from get_counts.R
      top_taxa = 10 #how many taxa to trim
      #attention! function remove all [taxa]*, keep only "S" and similar taxa


df_get_top_taxa: get top taxa from result data frame of different clade

      df, #result tibble from get_counts.R
      clade, #clade
      top, #count of top taxa to use


df_tidy_drop_unclassified: just drop all unclassified tables

      df, #result tibble from get_counts.R
      

df_rescale: rescale all amounts to 1. neccessary if you drop some taxa levels

      df, #result tibble from get_counts.R


df_get_parents: get parents taxas to df

      df, #result tibble from get_counts.R

      
df_drop_clade: additional function to get all unclassified level down

      df, #result tibble from get_counts.R


# Execute plots
df2donut(df, ...)           #ggplot2 donut plot
df2composition(df, ...)     #ggplot2 bar plot
df2barplot(df, ...)         #ggplot2 box plot. c:
df2cluster(df, k_means = 2) #base R cluster plot

df2clust2d(df, legend_detect, clade = F, k_means = 5, counts = F, ...)  #2D cluser visualisation using legend
df2heatmap(df, clade, trim = F, drop_unclassified = T, counts = F, ...) #base R heatmap plot
df2corrplot(df, clade = F, counts = F, ...)                             #corrplot. using scale
df2chord(df, clade = F, k_means = 5, counts = F,
         coenf_level = F, coenf, line_as_clusters = F, ...)             #ggraph and circlize connection plot
df2tsne(df, clade_trim, clade_color = "P", counts = T,
           taxa = NA, trim_taxa = T, only_input_taxa = F)               #ggplot2 tsne plot
df2pca_sample(df, clade = F, scale = T, ...)                            #factoextra pca plot for samples
df2pca_sp    (df, clade = F, scale = T, ...)                            #factoextra pca plot for taxa
         

    clade: character. which clade to use(e.g. "S", "G", "F")
    clade_color: character. which clade to usefor coloring (e.g. "S", "G", "F")
    drop_unclassified: logical. drop unclassified
    counts: logical. use counts or amount
    k_means: numeric. number of k-means to use
    legend_detect: character. what to detect in legend to divied by two groups
    coenf_level: logical or numeric. trim connections according to level
    coenf: character. one of "upper", "lower" or "both" to trim in coenf_level
    only_input_taxa: logical. use or not only inputed taxa for labels
    taxa: character. what taxa to show on labels
    trim_taxa: logical. get only 1 word from the taxa to label. better use for species
    scale: logical. scale or not the data to get z-scores
    

# References

Work on libraries: 
  - tidyverse
  - ggrepel
  - viridis
  - corrplot
  - ggraph
  - igraph
  - circlize
  - factoextra
  - Rtsne

df2tsne <- function(df, clade_trim, clade_color = "P", 
                    counts = T, log2df = T, taxa = NA, trim_taxa = T, only_input_taxa = F) {
  
  dfN <- df[df$clade == clade_trim,] %>% df_untidy(counts = counts)
  dfN <- dfN[,-1]
  dfN[is.na(dfN)] <- 0
  dfN <- unique(dfN)
  
  if(only_input_taxa) {
    dfN <- dfN[rownames(dfN) %in% taxa,]
  }
  
  if (log2df == T) dfN <- log2(dfN+1)
  
  dfK <- df_get_parents(df)[,c("taxa", clade_color)]
  dfK <- left_join(data.frame(taxa = rownames(dfN)),
                   dfK, by = "taxa")
  dfK <- unique(dfK)
  
  dfK$taxa[!(dfK$taxa %in% taxa)] <- NA
  
  if(trim_taxa == T) {
    dfK$taxa <- str_remove_all(dfK$taxa, "Candidatus ")
    dfK$taxa[!is.na(dfK$taxa)] %>% str_split(" ") %>% map(1) %>% unlist -> dfK$taxa[!is.na(dfK$taxa)]
    dfK$taxa[dfK$taxa %>% duplicated] <-  NA
  }
  
  lvir <- dfK[,2] %>% factor %>% levels %>% length
  
  print("preparation done...")
  
  tsne_out <- Rtsne(dfN)
  tsne_plot <- data.frame(x = tsne_out$Y[,1],
                          y = tsne_out$Y[,2])
  
  df_repel <- cbind(dfK, tsne_plot)
  df_repel <- df_repel[!is.na(df_repel$taxa),]
  
  
  print("tSNE calculated")
  
  ggplot() + 
    geom_point(aes(x=tsne_plot$x, y=tsne_plot$y, color = dfK[,2]), 
               show.legend = F, alpha = 0.5) +
    geom_label_repel(aes(label = df_repel$taxa, x = df_repel$x, y = df_repel$y), size = 5, max.overlaps = 100) +
    xlab("") + ylab("") +
    scale_color_discrete("", type = viridis(lvir)) +
    theme_minimal()
}
df2tsne <- function(df, color = "clust", k_means = 10, text_top = F) {
  
  #making cluster
  if (length(color) == nrow(df)) {
    groups <- data.frame(taxa = rownames(df),
                         clust = color)
  } else if (color == "clust") {
    clust_res <- df %>% dist %>% hclust(method = "ward.D2")
    
    groups <- data.frame(taxa = clust_res$labels,
                         clust = cutree(clust_res, k=k_means))
  } else {
    #DONT WORK NOW
    dfK <- df_get_parents(df)[,c("taxa", clade_color)]
    dfK <- left_join(data.frame(taxa = rownames(df)),
                     dfK, by = "taxa")
    dfK <- unique(dfK)
    
    dfK$taxa[!(dfK$taxa %in% taxa)] <- NA
  }

  #making tSNE
  tsne_out <- tsne::tsne(df)
  tsne_plot <- data.frame(taxa = rownames(df),
                          x = tsne_out[,1],
                          y = tsne_out[,2]) %>%
    left_join(groups, by = "taxa")
  
  if (text_top == -1) {
    tsne_plot$taxa <- NA
  } else if (text_top != F) {
    df <- df[order(apply(df, 1, mean), decreasing = T),]
    top <- rownames(df)[1:text_top]
    
    tsne_plot$taxa[!(tsne_plot$taxa %in% top)] <- NA
  }
  
  print("tSNE calculated")
  
  #plot
  tsne_plot$clust <- tsne_plot$clust %>% as.character %>% factor
  lvir <- tsne_plot$clust %>% levels %>% length
  
  ggplot(tsne_plot, aes(x, y)) + 
    geom_point(aes(color = clust), show.legend = T, alpha = 0.5) +
    geom_label_repel(aes(label = taxa), size = 5, max.overlaps = 100) +
    xlab("") + ylab("") +
    scale_color_discrete("", type = viridis(lvir)) +
    theme_minimal()
}
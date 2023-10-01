df2chord <- function(df, clade = F, k_means = 5, counts = F, log2_scale = F, ...) {
  
  #operations with df
  
  if(clade != F) {
    df <- df[df$clade == clade,]
  }
  
  df <- df %>% df_untidy(counts = counts)
  df[is.na(df)] <- 0
  df <- df[,-1]
  row.names(df) -> rdf
  
  if(log2_scale == T) {
    df <- df + 1
    df <- sapply(df, log2)
    row.names(df) <- rdf
  }
  
  df <- scale(df)
  
  #making cluster
  clust_res <- df %>% dist %>% hclust(method = "ward.D2")
  
  groups <- cutree(clust_res, k=k_means)

  df <- df[order(clust_res$order),]
  df <- df[order(groups),]
  groups <- groups[order(groups)]
  
  print("clusterization done")
  
  #making correlation
  df <- cor(t(df))
  
  #prepare connections
  ldft <- length(rdf)
  
  #remove duplicated
  h_remove <- c()
  for (i in 1:ldft) {
    h_remove <- c(h_remove, (ldft*(i-1)+1):(ldft*(i-1)+(i-1)))
  }
  h_remove <- h_remove[-(1:2)]
  
  #make connections
  vertices <- pivot_longer(as.data.frame(df), cols = 1:ldft)
  vals <- vertices[-h_remove,2] %>% unlist
  
  #making matrix
  gg <- graph_from_adjacency_matrix(df, mode = "undirected", weighted = T)
  gg <- ggraph(gg, layout = 'linear', circular = TRUE) + 
    geom_edge_arc(aes(alpha = (vals)^4, color = vals), show.legend = F) +
    geom_node_point(aes(x = x*1.05, y=y*1.05, color = as.character(groups)), show.legend = F) +
    scale_edge_color_gradient2(low="red", mid = "white", high="blue") +
    scale_edge_alpha_continuous(range = c(0, 0.5)) +
    coord_fixed() +
    theme_void()

  gg
}

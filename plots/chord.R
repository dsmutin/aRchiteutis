df2chord <- function(df, clade = F, k_means = 5, counts = F, log2_scale = F, 
                     coenf_level = F, coenf, line_as_clusters = F, ...) {
  
  #operations with df
  
  if(clade != F) {
    df <- df[df$clade == clade,]
  }
  
  df <- df %>% df_untidy(counts = counts)
  df[is.na(df)] <- 0
  df <- df[,-1]
  
  row.names(df) -> rdf
  ldfr <- length(rdf)
  
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
  
  #remove duplicated
  h_remove <- c()
  for (i in 1:ldfr) {
    h_remove <- c(h_remove, (ldfr*(i-1)+1):(ldfr*(i-1)+(i-1)))
  }
  h_remove <- h_remove[-(1:2)]
  
  #make connections
  vertices <- pivot_longer(as.data.frame(df), cols = 1:ldfr)
  
  #add angle and hjust
  angle <- 90 - 360 * 1:ldfr / ldfr
  hjust <- ifelse(angle < -90, 1, 0)
  angle <- ifelse(angle < -90, angle+180, angle)
  
  vals <- vertices[-h_remove,2] %>% unlist
  
  #making matrix
  gg <- graph_from_adjacency_matrix(df, mode = "undirected", weighted = T)
  
  if (line_as_clusters) {
    df_clust <- data.frame(name = rdf, group = groups)
    df_clust <- left_join(vertices, df_clust, by = "name")[-h_remove,3]
    
    if(coenf_level != F) {
      if(coenf == "upper") {
        df_clust[vals < coenf_level, 1] <- NA
      } else {
        if(coenf == "lower") {
          df_clust[vals > coenf_level, 1] <- NA
        } else {
          df_clust[abs(vals) < coenf_level, 1] <- NA
        }
      }
    }
    
    ggraph(gg, layout = 'linear', circular = TRUE) + 
      geom_edge_arc(aes(alpha = (vals)^4, color = as.character(df_clust$group)), show.legend = F) +
      geom_node_point(aes(x = x*1.05, y=y*1.05, color = as.character(groups)), show.legend = F) +
      geom_node_text(aes(x = x*1.1, y=y*1.1, label=name, angle = angle, hjust=hjust)) +
      scale_color_manual(values = viridis(k_means)) +
      scale_edge_color_manual(values = viridis(k_means), na.value = "transparent") +
      scale_edge_alpha_continuous(range = c(0, 0.5), na.value = 0) +
      coord_fixed() +
      theme_void() +
      expand_limits(x = c(-3, +3), y = c(-3, +3))
    
  } else {
    
    if(coenf_level != F) {
      if(coenf == "upper") {
        vals[vals < coenf_level] <- NA
      } else {
        if(coenf == "lower") {
          vals[vals > coenf_level] <- NA
        } else {
          vals[abs(vals) < coenf_level] <- NA
        }
      }
    }

    ggraph(gg, layout = 'linear', circular = TRUE) + 
      geom_edge_arc(aes(alpha = (vals)^4, color = vals), show.legend = F) +
      geom_node_point(aes(x = x*1.05, y=y*1.05, color = as.character(groups)), show.legend = F) +
      geom_node_text(aes(x = x*1.1, y=y*1.1, label=name, angle = angle, hjust=hjust)) +
      scale_color_manual(values = viridis(k_means)) +
      scale_edge_color_gradient2(low="red", mid = "white", high="blue", na.value = "transparent") +
      scale_edge_alpha_continuous(range = c(0, 0.5)) +
      coord_fixed() +
      theme_void() +
      expand_limits(x = c(-3, +3), y = c(-3, +3))
  }
}

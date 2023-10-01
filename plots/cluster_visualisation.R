df2clust2d<- function(df, legend_detect, clade = F, k_means = 5, counts = F, log2_scale = F, ...) {
  
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
  } else {
    df <- scale(df)
  }
  
  #making cluster
  clust_res <- df %>% dist %>% hclust(method = "ward.D2")
  
  groups <- cutree(clust_res, k=k_means)
  
  df <- df[order(clust_res$order),]
  df <- df[order(groups),]
  groups <- groups[order(groups)]
  
  #making tables for separate legend
  index <- df %>% colnames %>% str_detect(legend_detect) %>% which
  
  df1 <- df[,index] %>%
    as.data.frame %>%
    pivot_longer(cols = 1:length(index))
  df1$name <- rep(rdf, each = length(index))
  
  index <- df %>% colnames %>% str_detect(legend_detect, negate = T) %>% which
  df2 <- df[,index] %>%
    as.data.frame %>%
    pivot_longer(cols = 1:length(index))
  df2$name <- rep(rdf, each = length(index))

  
  df1mean <- summarise(df1, mean(value), .by = "name")
  df2mean <- summarise(df2, mean(value), .by = "name")
  
  res <- data.frame(name = df1mean$name,
                    x = df1mean$`mean(value)`,
                    y = df2mean$`mean(value)`)
  res$name[abs(res$x/res$y) < 1.1 & abs(res$x/res$y) > (1/1.1)] <- NA
  
  
  ggplot(res, aes(x, y)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "cadetblue") +
    geom_point(aes(x, y, color = groups)) +
    geom_label_repel(aes(label = name)) +
    scale_color_continuous("Cluster", type = "viridis") +
    coord_fixed() +
    theme_minimal ()
  
}

df2heatmap <- function(df, clade, trim = F, drop_unclassified = T, counts = F, ...){
  df <- df[df$clade == clade,] %>% df_untidy(counts = counts, drop_unclassified = drop_unclassified)
  df_names <- row.names(df)
  df <- df[,-1] %>% sapply(as.numeric)
  df[is.na(df)] <- 0
  row.names(df) <- df_names
  
  
  if(trim != F) {
    df_sum <- apply(df, 1, sum)
    df <- df[order(df_sum, decreasing = T),]
    df <- df[1:trim,]
  }
  
  df <- as.matrix(df)
  
  heatmap(df, col = rev(viridis(256)), ...)
}

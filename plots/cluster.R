df2cluster <- function(df, k_means = 2) {
  df <- df %>% df_untidy(counts = F)
  df <- df[,-1]
  df[is.na(df)] <- 0
  
  df <- scale(df)
  clust_res <- df %>% dist %>% hclust(method = "ward.D2")
  
  plot(clust_res)
  rect.hclust(clust_res, k=k_means, border="red")
}

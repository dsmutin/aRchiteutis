df2cluster <- function(df, k_means = 2, use = "sp") {
  rdf <- rownames(df)
  df <- apply(df, 2, scale)
  row.names(df) <- rdf
  
  if (use == "sp") {
    clust_res <- df %>% dist %>% hclust(method = "ward.D2")
  } else {
    clust_res <- df %>% t %>% dist %>% hclust(method = "ward.D2")
  }
  
  plot(clust_res)
  rect.hclust(clust_res, k=k_means, border="red")
}

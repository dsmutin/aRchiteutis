df2pca_sample <- function(df, scale = T, detect = F, detect2 = "other", ...) {

  res.pca <- prcomp(t(df), scale = scale)
  
  if (detect != F) {
    col_list <- colnames(df) %>% str_detect(detect)
    color_list <- col_list
    color_list[col_list] <- detect
    color_list[!col_list] <- detect2
  }
  
  fviz_pca_ind(res.pca, col.ind = color_list, ...) +
    theme_minimal() +
    ggtitle("")
}


df2pca_sp <- function(df, scale = T, ...) {

  res.pca <- prcomp(t(df), scale = scale)
  
  fviz_pca_var(res.pca, ...) +
    theme_minimal() +
    ggtitle("")
}

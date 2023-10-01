df2pca_sample <- function(df, clade = F, scale = T, ...) {
  
  if(clade != F) {
    df <- df[df$clade == clade,]
  }
  
  df <- df %>% df_untidy 
  df[is.na(df)] <- 0
  df <- df[,-1]
  
  res.pca <- prcomp(t(df), scale = scale)
  
  fviz_pca_ind(res.pca,...) +
    theme_minimal() +
    ggtitle("")
}


df2pca_sp <- function(df, clade = F, scale = T, ...) {
  
  if(clade != F) {
    df <- df[df$clade == clade,]
  }
  
  df <- df %>% df_untidy 
  df[is.na(df)] <- 0
  df <- df[,-1]
  
  res.pca <- prcomp(t(df), scale = scale)
  
  fviz_pca_var(res.pca,...) +
    theme_minimal() +
    ggtitle("")
}

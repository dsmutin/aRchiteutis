df2corrplot <- function(df, clade = F, counts = F, log2_scale = F, ...) {
  
  if(clade != F) {
    df <- df[df$clade == clade,]
  }
  
  df <- df %>% df_untidy(counts = counts)
  df[is.na(df)] <- 0
  df <- df[,-1]
  
  if(log2_scale == T) {
    row.names(df) -> rdf
    df <- df + 1
    df <- sapply(df, log2)
    row.names(df) <- rdf
  }
  
  df <- scale(df)
  
  df <- cor(t(df))
  
  corrplot(df, hclust.method = "ward.D2", tl.col = "black", ...)
}

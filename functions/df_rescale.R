df_rescale <- function(df) {
  df_sum <- summarise(df, sum(N), .by = "sample")
  
  for (i in levels(df$sample)) {
    df[df$sample == i,5] <- df[df$sample == i,4] / unlist(df_sum[df_sum$sample ==i,2])
  }
  
  print("rescaling done...")
  
  return(df)
}

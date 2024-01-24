df2heatmap <- function(df, clade, trim = F, ...){
  heatmap(t(df), col = rev(viridis(256)), ...)
}

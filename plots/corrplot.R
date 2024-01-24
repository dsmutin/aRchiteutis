df2corrplot <- function(df, k_means = F, ...) {
  df1 <- cor(t(df))
  df2 <- cor.mtest(t(df))
  
  corrplot(df1, is.corr = T, hclust.method = "complete", tl.col = "black", order = "hclust", addrect = k_means, font = 3, ...)
}

df_smart_bind <- function(f1, ...) {
  print("Unclassified amount parsed by 1st file")
  
  df <- rbind(f1, ...)
  
  #amount processing
  df_sum <- f1 %>% subset(clade %in% c("U", "R")) %>% summarise(sum(N), .by = "sample")
  #df_sum2 <- df %>% subset(clade %in% c("U", "R")) %>% summarise(sum(N), .by = "sample")
  #df_sum <- left_join(df_sum, df_sum2, by = "sample")
  #df_sum <- data.frame(sample = df_sum$sample, sum = apply(df_sum[,-1], 1, max))
  
  colnames(df_sum) <- c("sample", "sum")
  
  df_cl_sum <- df %>% subset(clade == "R") %>%
    summarise(sum(N), .by = "sample")
  
  df_sum <- left_join(df_sum, df_cl_sum, by = "sample")
  
  #add unclassified correctly
  uncl <- f1[f1$clade == "U",]
  df_uncl <- data.frame(sample = df_sum$sample,
                        sum = df_sum$sum - df_sum$`sum(N)`,
                        am = (df_sum$sum - df_sum$`sum(N)`)/df_sum$sum)
  uncl <- left_join(uncl, df_uncl, by = "sample")
  uncl$N <- uncl$sum
  uncl$amount <- uncl$am
  uncl$amount_cl <- 0
  
  df <- df[df$clade != "U",] %>% left_join(df_sum, by = "sample")
  df$amount <- df$N / df$sum
  df$amount_cl <- df$N / df$`sum(N)`
  
  df <- df %>% bind_rows (uncl)
  df[-((length(df[1,])-2):length(df[1,]))]
  
}
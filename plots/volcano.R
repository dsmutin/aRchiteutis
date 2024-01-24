df2volcano <- function(df, legend_detect, treshhold_logAC = 0.5, treshhold_p = 0.05) {
  
  #perform t-test
  df_un <- df %>% df_untidy(keep_sample_name = F)
  df1 <- df_un[,colnames(df_un) %>% str_detect(legend_detect[1])]
  df2 <- df_un[,colnames(df_un) %>% str_detect(legend_detect[2])]
  
  t_df <- c()
  for (i in 1:length(df_un[,1])) {
    if ((sum(df1[i,]) != 0) & (sum(df2[i,]) != 0)) {
      t_df <- c (t_df, t.test(df1[i,], df2[i,], paired = F)$p.value)
    }  else {
      t_df <- c(t_df, NA)
    }
  }
  
  t_df <- data.frame(taxa = row.names(df_un),
                     p = -log10(t_df), 
                     sd = apply(df_un, 1, sd))
  
  #making tables for separate legend
  df_str <- apply(df[, -c(1:6)], 1, str_c, collapse = "_")
  df1mean <- summarise(df[df_str %>% str_detect(legend_detect[1]),], mean(amount), .by = "taxa")
  df2mean <- summarise(df[df_str %>% str_detect(legend_detect[2]),], mean(amount), .by = "taxa")
  
  res <- full_join(df1mean, df2mean, by = "taxa") %>% left_join(t_df, by = "taxa") %>% as.data.frame
  colnames(res)[2:3] <- c("x", "y")
  res[is.na(res)] <- 0
  res$logAC <- log10(res$y/res$x)
  
  res$taxa[(abs(res$logAC) < treshhold_logAC) | (res$p < -log10(treshhold_p))] <- NA
  
  
  ggplot(res, aes(logAC, p)) +
    geom_point(aes(color = abs(logAC*p), size = x+y), show.legend = F) +
    geom_hline(yintercept = -log10(treshhold_p), linetype = 3, alpha= 0.5, color = "red") +
    geom_vline(xintercept = treshhold_logAC, linetype = 3, alpha= 0.5, color = "red") +
    geom_vline(xintercept = -treshhold_logAC, linetype = 3, alpha= 0.5, color = "red") +
    geom_label_repel(aes(label = taxa), max.overlaps = 30) +
    scale_color_gradient("logAC", high = "blue", low = "gray", na.value = "blue") +
    xlab("log10 amount change") + ylab ("p-value by t.test") +
    theme_minimal ()
  
}

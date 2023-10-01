df2barplot <- function(df, ...) {
  lvir <- df$taxa %>% factor %>% droplevels %>% levels %>% length
  
  df_sum <- summarise(df, mean(amount), .by = "taxa")
  df <- left_join(df, df_sum, by = "taxa")
  
  df <- df[order(df$`mean(amount)`),]
  df$taxa <- fct_inorder(df$taxa)
  
  ggplot(df, aes(amount, taxa, fill = taxa)) +
    geom_boxplot(..., show.legend = F) +
    theme_minimal() +
    xlab("") + ylab("") +
    scale_fill_discrete("", type = viridis(lvir))
}

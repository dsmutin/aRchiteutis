df_get_top_taxa <- function(df, clade, top) {
  taxa <- summarise(df[df$clade == clade,], mean(amount), .by = "taxa")
  taxa <- taxa[order(taxa$`mean(amount)`, decreasing = T),1]
  taxa <- taxa[1:top,1] %>% unlist %>% as.character
}

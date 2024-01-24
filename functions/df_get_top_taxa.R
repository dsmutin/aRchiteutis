df_get_top_taxa <- function(df, drop_unclassified = T, clade = F, top = 50) {
  
  if (clade != F) df <-df[df$clade == clade,]
  if(drop_unclassified) df <- df %>% df_tidy_drop_unclassified
  
  taxa <- summarise(df, mean(amount), .by = c("taxa", "clade"))
  taxa <- taxa[order(taxa$`mean(amount)`, decreasing = T),]
  taxa <- taxa[1:top,-3] %>% apply(1, str_c, collapse = "_") %>% unlist %>% as.character
  df_taxa <- df[,1:2] %>% apply(1, str_c, collapse = "_")
  df <- df[df_taxa %in% taxa,]
  
}

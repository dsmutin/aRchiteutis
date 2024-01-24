df_untidy <- function (
    df, #result tibble from get_counts.R
    clade = F,
    amount_from = "amount",
    top = F,
    scale = F, #eval function, etc log2 or scale
    drop_unclassified = F,
    keep_sample_name = T #warning! if T translate legend to separated format in name of each column :(
    
    #get untidy table for base::heatmap or future processing
) {
  
  if(drop_unclassified) df <- df %>% df_tidy_drop_unclassified
  
  if(clade != F) {
    df <- df[df$clade == clade,]
  }
  
  if(!keep_sample_name) {
    df$sample <- df[,c(7:length(df[1,]), 3)] %>% apply(1, str_c, collapse = "_")
  }
  
  #edit duplicates
  df <- df[,c("taxa", "sample", amount_from)]
  colnames(df)[3] <- "N"
  
  df <- summarise(df, sum(N), .by = c("taxa", "sample"))
  
  res <- df %>%
    pivot_wider(values_from = `sum(N)`, names_from = sample, id_cols = taxa)
  
  rn_res <- res$taxa
  res <- res[,-1] %>%
    sapply(as.numeric) %>%
    as.matrix
  res[is.na(res)] <- 0
  rownames(res) <- rn_res
  
  if(top != F) {
    df_sum <- apply(res, 1, sum)
    res <- res[order(df_sum, decreasing = T),]
    res <- res[1:top,]
  }
  
  if (scale != F) {
    row.names(res) -> rdf
    if (scale == "log2") res <- log2(res + 1)
    if (scale == "scale") res <- res %>% apply(2, scale)
    row.names(res) <- rdf
  }
  
  return(res)
}

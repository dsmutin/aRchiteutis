df_untidy <- function (
    df, #result tibble from get_counts.R
    
    counts = T, #use counts or amount data
    classified_only = F, #only if counts = F, get amount by classified data
    drop_unclassified = F, #drop all unclasified levels
    keep_sample_name = F #warning! if T translate legend to separated format in name of each column :(
    
    #get untidy table for base::heatmap or future processing
) {
  res <- data.frame("taxa" = NA, "clade" = NA)
  
  #i know that pivot_wider() is better but what else)
  
  #what column to transform in table
  if (counts == T) {
    df_column_add <- 4
  } else {
    if(classified_only == F) {
      df_column_add <- 5
    } else {
      df_column_add <- 6
    }
  }
  
  for (fct in levels(df$sample)) {
    df2 <- df[df$sample == fct, c(1,2,df_column_add)]
    
    #apply legend
    if ((length(df[1,]) > 6 ) & (keep_sample_name == F)){
      df2name <- df[df$sample == fct,][1,-c(1:6)]
      df2name <- df2name %>% data.frame %>% sapply(as.character)  %>% unlist %>% paste(collapse = "_")
      #ehh, TIBBLE)
    } else {
      df2name <- df[df$sample == fct,][1,3] %>% as.character
    }
    
    colnames(df2)[3] <- df2name
    
    res <- full_join(res, df2, by = c("taxa", "clade"))
  }
  
  #rename rows
  res <- res[-1,]
  
  if(drop_unclassified == T) {
    res_uncl <- res$taxa %>% str_detect(" unclassified") %>% which
    res <- res[-res_uncl,]
  }
  
  rtaxa <- res$taxa
  res[is.na(res)] <- 0
  res <- res[,-c(1:2)]
  res <- sapply(res, as.numeric)
  rownames(res) <- rtaxa
  
  res <- res[,apply(res, 2, sum) > 0]
    
  print("wider table done")
  return(res)
}

df_taxa_trim <- function (
    df, #result tibble from get_counts.R
    top_taxa = 10 #how many taxa to trim
    #attention! function remove all [taxa]*, keep only "S" and similar taxa
){
  clade_chr <- c("U","R","D","P","C","O","F","G","S")
  
  df2 <- df %>% df_get_parents
  
  #get summarized table in decreasing order
  df2 <- summarise(df2, mean(amount), .by = c("taxa","clade", clade_chr[2:8])) %>% as.data.frame
  df2 <- df2[order(df2[,10], decreasing = T),-10]
  df2 <- apply(df2, 2, as.character) %>% as.data.frame
  
  res <- df2[1,]
  
  #adding taxa to result table and checking parents
  i = 1
  while (length(res[,1]) < (top_taxa - 1)) {
    
    res_line <- df2[i,]
    df_parents <- res[,clade_chr[2:8]] %>% unlist %>% as.factor %>% droplevels %>% levels
    
    if(!(res_line$taxa %in% df_parents)) {
      res <- res[!(res$taxa %in% res_line[,clade_chr[2:8]]),]
      res <- res %>% rbind(res_line)
    }
    
    i = i + 1
  }
  
  #working with "other" groups
  df_other <- df[df$clade == "D",] 
  df_other <- df_other[!(df_other$taxa %>% str_detect(" unclassified")),]
  df_other$taxa <- str_c("other ", df_other$taxa)

  #making final table
  df_res <- df[df$taxa %in% res$taxa,]
  df_sum <- df_res   %>% 
    summarise(sum(N), sum(amount), sum(amount_cl),.by = c("sample"))
  
  #calculating "other"
  df_other <- left_join(df_other, df_sum, by = "sample")
  df_other[,4:6] <- df_other[,4:6] - df_other[,(length(df_other[1,])-2):length(df_other[1,])]
  df_other <- df_other[,-((length(df_other[1,])-2):length(df_other[1,]))]
  
  #add unclassified and other
  df_res <- rbind(df_res, df[df$taxa == "unclassified",], df_other)
  
  print("taxa trimmed...")
  return(df_res)
}

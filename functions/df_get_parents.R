df_get_parents <- function (
    df #result tibble from get_counts.R
    
    #add several columns with parents taxa
){
  
  df <- df_drop_clade(df)
  res <- df
  df <- df %>% summarise(.by = c("taxa", "clade"))
  
  
  #factor of k2 clade
  clade_chr <- c("U","R","D","P","C","O","F","G","S")

  
  #make parents taxa present in table
  for (i in clade_chr[-c(1,9)]) {
    
    df[,i] <- NA
    df_taxa <- (df$clade == i) %>% which
    df_taxa <- c(df_taxa, length(df$clade) + 1)
    
    if (length(df_taxa) > 1) {
      for (j in 1:(length(df_taxa)-1)) {
        df[(df_taxa[j] + 1):(df_taxa[j+1]-1),i] <- df$taxa[df_taxa[j]]
      }
    }
    
  }
  
  #remove redundant parents info
  df <- df[!(df$taxa %>% is.na),]
  
  for (i in 2:7) {
    df[df$clade == clade_chr[i], clade_chr[(i+1):8]] <- NA
  }
  
  #apply normal legend to unclassified
  df_uncl <- df$taxa %>% str_detect("unclassified") %>% which
  
  for (i in df_uncl) {
    #find clade for each unclassified
    df_uncl_N <- df[i,1] %>% unlist %>% as.character %>% str_remove(" unclassified") 
    df_uncl_leg <- (df[,1] == df_uncl_N) %>% which
    df[i, clade_chr[2:8]] <- df[df_uncl_leg[1], clade_chr[2:8]]
    
    df_uncl_na <- is.na(df[i, clade_chr[2:8]])
    if(sum(df_uncl_na) > 0) {
      df[i, clade_chr[(df_uncl_na %>% which)[1] + 1]] <- 
        df[i, 1] %>% unlist %>% as.character %>% str_remove(" unclassified")
    }
    
  }
  
  res <- left_join(res, df, by = c("taxa", "clade"))
  
  print("classification done")
  return(res)
}

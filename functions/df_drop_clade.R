df_drop_clade <- function (
    df #result tibble from get_counts.R
    
    #drop each level out of "U-S" and leveling down all unclassified
){
  
  #factor of k2 clade
  clade_chr <- c("U","R","D","P","C","O","F","G","S")
  
  #removing few clade
  df <- df[df$clade %in% clade_chr,]
  df$clade <- df$clade %>% as.character
  
  #change level of unclassified
  df_uncl <- df$taxa %>% str_detect("unclassified") %>% which
  
  for (i in 9:1) {
    df_rename <- (df$clade == clade_chr[i]) %>% which 
    df_rename <- df_rename[df_rename %in% df_uncl]
    
    df [df_rename,2] <- c(clade_chr[-1], "remove")[i]
  }
  
  #drop unclassified S1
  df <- df[!(df$clade == "remove"),]
  
  print("few clade levels was dropped...")
  return(df)
}
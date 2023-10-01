#libs ----
library("tidyverse")

#main----
get_counts <- function (
    path, #path to k2 reports
    keep_unclassified = T, #keep or not unclassified reads from report as new taxa
    pattern = "", #optional: pattern of file names to use
    trim_char = F, #optional: character to split file name in final table
    clade = F, #optional: which clade (like "O", "F","G", "S" to use)
    type = k2, #type, now only available for kraken2
    legend = F #optional: path to legend file in csv format: 
        #legend$name == name of kraken report (OR slited file name!), rest of columns - legend itself
    
    #result is data.frame with columns: 
        #taxa, 
        #clade, 
        #sample, 
        #number of reads, 
        #ammount of all reads, 
        #amount of classified reads 
        #and several columns of applied legend
) {
  
  #variables----
  
  file_list <- dir(path, pattern = pattern)
  res <- data.frame()
  
  
  #reading and processing of files----
  
  for (file in file_list) {
    
    df <- readLines(paste0(path,"/",file))
    
    if (clade != F) { #for_separate_clade
      dfN <- str_detect(df, clade) %>% which
      df <- df[dfN] %>% str_split("\t") %>% as.data.frame %>% t %>% as.data.frame
      
    } else {
      df <- df %>% str_split("\t") %>% as.data.frame %>% t %>% as.data.frame
      
    }
    
    #normal names for df
    colnames(df) <- c("amount", "N", "NUnCl", "clade", "id", "taxa")
    
    df$taxa <- df$taxa %>% str_trim ("both")
    row.names(df) <- df$id
    
    #change classes
    df[,2:3] <- df[,2:3] %>% sapply (as.numeric)
    df[,4] <- as.factor(df[,4])
    df[,6] <- as.factor(df[,6])
    
    
    #making unclassified ranks
    if(keep_unclassified == T) {
      df_uncl <- (df$NUnCl != 0) %>% which
      df_uncl <- df[df_uncl,]
      df_uncl$N <- df_uncl$NUnCl #check for errors
      df_uncl$taxa <- str_c (df_uncl$taxa, " unclassified")
      rownames(df_uncl) <- rownames(df_uncl) %>% str_c("_1")
      df <- rbind(df, df_uncl[-1,])
    }

    
    #remove columns and scaling
    df <- df[,-c(3,5)]
    df_sum <- sum(df[c("0","1"),2])
    df$amount <- df$N / df_sum
    df$amount_cl <- df$N / df[c("1"),2]
    
    #add sample and trim file name
    if (trim_char != F) file <- str_split(file, trim_char) %>% map(1) %>% unlist
    df$sample <- file
    df <- df[,c(4,3,6,2,1,5)]
    
    res <- rbind(res, df)
    
    print(paste0(file, " done"))
  }
  
  #add legend ----
  if (legend != F) {
    legend <- read.csv(legend, header = T, row.names = 1, stringsAsFactors = T)
    
    #remove trim_char from legend
    if (trim_char != F) {
      legend$sample <- row.names(legend) %>% str_split(trim_char) %>% map(1) %>% unlist
    } else {
      legend$sample <- row.names(legend)
    }
    
    res <- left_join(res, legend, by = "sample")
    
    print("Legend applied")
  }
  
  #a few data corrections
  res$sample <- as.factor(res$sample)

  print("all files added to table...")
  return(as_tibble(res))
}

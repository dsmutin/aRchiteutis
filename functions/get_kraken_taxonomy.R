get_kraken_taxonomy <- function(path,
                                pattern = "",
                                extract_taxa = F, #vector of taxa belong to same classification level to extract
                                extract_clade #character, classification level to work with
                                ) {
  #variables----
  
  file_list <- dir(path, pattern = pattern)
  res <- data.frame()
  
    for (file in file_list) {
      
      df <- read.table(paste0(path,"/",file), sep = "\t", fill = T, quote = "")
      
      #normal names for df
      colnames(df)[1:6] <- c("amount", "N", "NUnCl", "clade", "id", "taxa")
      
      df <- df[,c("clade", "taxa")]
      df$taxa <- df$taxa %>% str_remove("^ *")
      
      if(!isFALSE(extract_taxa)) {
        c_list <- which(df$clade == extract_clade)
        t_list <- which(df$taxa %in% extract_taxa)
        t_list <- t_list[t_list %in% c_list]
        tcint <- c_list %in% t_list
        
        #work with end of the table
        if(tcint[length(tcint)]){
          tcint <- c(tcint, F)
          c_list <- c(c_list, nrow(df)+1)
        }
        
        c_list[!tcint] <- c_list[!tcint] - 1
        
        keep_df <- data.frame(n1 = c_list[tcint],
                              n2 = c_list[c(F, tcint[-length(tcint)])])
        
        #seqs
        keep_num <- c()
        for (i in nrow(keep_df)) {
          keep_num <- c(keep_num, keep_df$n1[i]:keep_df$n2[i])
        }
        
        df <- df[keep_num,]
      }
      
      res <- rbind(res, df)
      cat(file, " done\n")
    }
  res <- unique(res)
}
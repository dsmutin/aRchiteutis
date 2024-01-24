#libs ----
library("tidyverse")

#main----
get_counts <- function (
    path, #path to k2 reports
    keep_unclassified = T, #keep or not unclassified reads from report as new taxa
    pattern = "", #optional: pattern of file names to use
    trim_char = F, #optional: character to split file name in final table
    output_type = "kraken2", #character. type of output file. one of kraken2, kaiju
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
  
  #kraken2 ----
  if (output_type == "kraken2") {
    
    
    #reading and processing of files----
    
    for (file in file_list) {
      
      df <- read.table(paste0(path,"/",file), sep = "\t", fill = T, quote = "")
      
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
    
  }
  
  #kaiju ----
  if (output_type == "kaiju") {
    
    for (file in file_list) {
      
      df <- read.table(paste0(path, "/", file), 
                       sep = "\t", header = F)
      
      df_names <- df$V4 %>% str_split("; ")
      df_names <- as.data.frame(do.call(rbind, df_names))
      
      #remove errors in rbind
      ldfn <- length(df_names[1,])
      main <- df_names[1,1]
      df_taxa_vector <- c()
      df_genus_vector <- c()
      
      for (i in 1:length(df_names[,1])){
        if (sum(df_names[i,-1] %in% c("", main)) > 0) {
          df_clear <- (df_names[i,-1]  %in% c("", main)) %>% which + 1
          df_names[i, df_clear[1]:ldfn] <- NA
        }
        df_taxa <- df_names[i,] %>% is.na %>% which
        df_taxa_vector <- df_taxa_vector %>% c(df_names[i, df_taxa[1]-1])
        
        #get genus
        df_genus <-((df_names[i,] %>% str_detect (" ")) & !(
          (df_names[i,] %>% str_detect ("incertae sedis")) |
            (df_names[i,] %>% str_detect ("species")) |
            (df_names[i,] %>% str_detect ("cellular")))) %>% which
        
        df_genus <- df_names[i,max(df_genus)] %>% str_split(" ") %>% map(1) %>% unlist
        if(is.null(df_genus)) df_genus <- paste0("unclassified ", df_names[i,6])
        
        df_genus_vector <- df_genus_vector %>% c(df_genus)
      }
      df_genus_vector[df_genus_vector == "unclassified NA"] <- "unclassified"
      
      
      print (paste0("names from ", file, " parsed..."))
      
      df <- data_frame(taxa = df_genus_vector,
                       clade = "G",
                       sample = file,
                       N = 1)
      
      df <- summarise(df, sum(N), .by = c("taxa", "clade", "sample"))
      
      colnames(df)[4] <- "N"
      df$amount <- df$N / sum(df$N)
      
      #maybe specify in future
      df$amount_cl <- df$amount
      
      res <- rbind(res, df)
    }
    
   res$sample <- res$sample %>% str_split(trim_char) %>% map(1) %>% unlist
  }
  
  #bracken ----
  if(output_type == "bracken") {
    for (fname in file_list) {
      
      df <- read.table(paste0(path, "/", fname), 
                       sep = "\t", header = T)
      
      if (!isFALSE(trim_char)) fname <- fname %>% str_split(trim_char) %>% map(1) %>% unlist
      
      df <- data.frame(taxa = df$name,
                       clade = df$taxonomy_lvl,
                       sample = fname,
                       N = df$new_est_reads,
                       amount = df$fraction_total_reads)
      df$amount_cl <- df$N / sum(df$N)
      
      res <- rbind(res, df)
      
      cat(fname, " done\n")
      
    }
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

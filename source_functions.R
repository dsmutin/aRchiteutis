#libs ----
liblist <- c("tidyverse",
             "ggrepel",
             "corrplot",
             "viridis",
             "Rtsne",
             "circlize",
             "factoextra",
             "ggraph",
             "igraph")

for (i in liblist) {
  if (!require(i, quietly = T, character.only = T))
    install.packages(i)
  library(i, character.only = T)
}


#source functions ----
functions <- c(str_c("functions/", dir("functions")), 
               str_c("plots/", dir("plots")))

for (i in functions) source(i)

rm(functions, liblist, i)

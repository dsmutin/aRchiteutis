df2beta <- function(df, 
                    clade = "G",
                    dist_function = bray_curtis, 
                    treshhold_up = 1,
                    treshhold_down = 0,
                    add_legend = F, #column number(s) to add legend
                    add_labels= F, #column number(s) to add legend
                    print_df = F,
                             ...) {
  
  name2viridis <- function(name) {
    name <- name %>% unlist %>% as.character
    n_un <- data.frame(
      name = unique(name),
      col = viridis(length(unique(name))) )
    
    name <- data.frame(name = name) %>% left_join(n_un, "name")
    name[,2] %>% unlist %>% as.character
  }
  
  #pallete <- colorRampPalette(c("white", "lightyellow",rev(viridis(4))))(254)
  #pallete <- colorRampPalette(c("white", "lightyellow",rev(plasma(6)[-c(1:2)])))(254)
  #pallete <- colorRampPalette(c("white", "lightyellow", rev(heat.colors(3, alpha = 0.7))))(254)
  pallete <- colorRampPalette(c("white", "lightyellow", "orange", "orangered3","darkred"))(254)
  pallete <- rev(pallete)
  #pallete <- heat.colors(254, rev = T)
  
  if(!is.null(clade)) df <- df[df$clade %in% clade,]
  
  if(paste0(add_legend, collapse = "") != F) {
    leg <- unique(df[,c(3,add_legend)])
    colsides <- leg[,-1] %>% apply(2, name2viridis)
  } else {
    colsides = NULL
  }
  
  if(paste0(add_labels, collapse = "") != F) {
    leg2 <- unique(df[,c(3,add_labels)])
    leg2 <- leg2[,-1] %>% apply(1, str_c, collapse = ", ")
  } else {
    leg2 = unique(df$sample)
  }
  
  #remove taxa out of treshholds
  df_taxa <- df %>% summarise(mean(amount_cl), .by = taxa)
  df_taxa <- df_taxa %>% 
    subset(`mean(amount_cl)` < treshhold_up) %>%
    subset(`mean(amount_cl)` > treshhold_down) 
  
  df_taxa <- df_taxa[,1] %>% unlist %>% as.character
  
  df <- df %>%
    subset(taxa %in% df_taxa)
  
  if(print_df) return(df %>%
                             df_untidy(drop_unclassified = T, scale = F) %>%
                             as.data.frame %>%
                             lapply(function(x) x/sum(x)) %>%
                             as.data.frame %>%
                             t %>%
                             dist_make(dist_function) %>%
                             as.matrix)
  
  gg <- function (df, ...) {
    df2 <- 
      df %>%
      df_untidy(drop_unclassified = T, scale = F) %>%
      as.data.frame %>%
      lapply(function(x) x/sum(x)) %>%
      as.data.frame %>%
      t %>%
      dist_make(dist_function) %>%
      as.matrix 
    df2[1,1] <- 1
    
    df2 %>%
    heatmap3(symm = T, 
             #margins = c(20,20),
              col = c("white",pallete, "white"), 
              showRowDendro = F,
              labRow = as.expression(lapply(leg2, function(a) bquote(italic(.(a))))),
              labCol = as.expression(lapply(leg2, function(a) bquote(italic(.(a))))),
              method = "ward.D2",
              cexCol = 1.5,
              cexRow = 1.5, 
              ...)
  }
  
  #add colors
  if (!is.null(colsides)) {
    gg(df, ColSideColors = colsides)
  } else {
    gg(df)
  }
  #legend(x="right", legend=seq(0,1,0.1),fill=plasma(11))
}

df2beta_pcoa <- function(df,
                         dist_function = bray_curtis,
                         treshhold_up = 1,
                         treshhold_down = 0,
                         add_legend = F, #column name
                         add_ellipse = F, #column name
                         ...) {

  df_legend <- unique(df[,-c(1:2,4:6)])
  
  ns <- function(a) { if(a == 3) { 1 } else { a - 5 } }
  
  if(!isFALSE(add_legend)) {
    leg1 <- df_legend[,ns(add_legend)] %>% apply(2, str_c, sep = "_") %>% unlist
  } else {
    leg1 <- F
  }
  
  if(!isFALSE(add_ellipse)) {
    leg2 <- df_legend[,ns(add_ellipse)] %>% apply(2, str_c, sep = "_") %>% unlist
  } else {
    leg2 <- NULL
  }
  
  pcoa_df <- df %>%
    df_untidy(drop_unclassified = T, scale = F) %>%
    as.data.frame %>%
    t %>%
    dist_make(dist_function) %>%
    as.matrix %>%
    pcoa()
  
  gg <- as.data.frame(pcoa_df$vectors) %>%
    ggplot(aes(Axis.1, Axis.2)) +
    geom_point(aes(color = fct_inorder(leg1))) +
    scale_color_discrete(NULL, type = viridis(length(unique(leg1)))) +
    theme_minimal() 
  
  if(!is.null(add_ellipse)) {
    gg +
      new_scale_colour() +
      geom_mark_ellipse(aes(color = leg2, 
                            label = leg2
                            ),
                        label.buffer = unit(-5, 'mm'))
  }
  
}
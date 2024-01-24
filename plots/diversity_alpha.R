df2alpha_summary <- function(df, 
                             split_by = F, #column number to split the plot
                             add_legend = F, #column number(s) to add legend.
                             ...) {
  gg <- df %>%
    group_by(sample) %>%
    summarise(across(N, list(
      Dominance = dominance,
      Simpson = simpson,
      "Simpson's evenness" = simpson_e,
      "Inverted simpson" = invsimpson,
      
      Shannon = shannon,
      "Brillouin's dominance" = brillouin_d,
      "Heip's evenness" = heip_e,
      "Pielou's evenness" = pielou_e,
      
      "Strong’s dominance" = strong,
      "Mcintosh's dominance" = mcintosh_d,
      "Berger&Parker's dominance" = berger_parker_d,
      Richness = richness,
      
      "Mcintosh's evenness" = mcintosh_e,
      "Menhinick’s richnes" = menhinick,
      "Margalef’s richness" = margalef,
      "Kempton&Taylor Q" = kempton_taylor_q

    )))
  
  gg <- gg %>% pivot_longer(cols = -1)
  
  gg$name <- gg$name %>% str_remove_all("N_") %>% factor %>% fct_inorder
  
  if(!isFALSE(add_legend)) {
    leg <- unique(df[,c(3,add_legend)])
    leg[,2] <- leg[,-1] %>% apply(1, str_c, collapse = ", ")
    gg <- left_join(leg[,1:2], gg, by = "sample")
    colnames(gg)[2] <- "sample2"
    
  } else {
    leg = unique(df[,3])
    gg$sample2 <- gg$sample
  }
  
  if (!isFALSE(split_by)) {
    
    leg <- unique(df[,c(3,split_by)])
    colnames(leg)[2] <- "split"
    gg <- left_join(leg[,1:2], gg, by = "sample")
    #gg$sample2 <- fct_inseq(gg$sample2)

    ggplot(gg, aes(y = value, x = split)) +
      geom_violin(trim = F, draw_quantiles = 0.5, aes(fill = split), alpha = 0.3) +
      geom_jitter(aes(color = sample2), width = 0.1) +
      facet_wrap(~name, drop = T, ncol = 4, scales = "free", strip.position = "top") +
      scale_color_discrete(NULL, 
                           type = viridis(length(unique(gg$sample2)))) +
      scale_fill_discrete(NULL) +
      ylab("") + xlab ("") +
      theme_minimal() +
      theme(legend.position = "right", 
            #axis.text.y = element_blank(),
            axis.text.x = element_blank())
  } else {
    ggplot(gg, aes(x = value, y = name)) +
      geom_violin(trim = F, draw_quantiles = 0.5) +
      geom_jitter(aes(color = sample2), width = 0.1) +
      facet_wrap(~name, drop = T, ncol = 4, scales = "free", strip.position = "top") +
      scale_color_discrete("", 
                           type = viridis(length(unique(gg$sample))), 
                           labels = as.character(unlist(leg[,-1]))) +
      ylab("") + xlab ("") +
      theme_minimal() +
      theme(legend.position = "right", 
            #axis.text.y = element_blank()
            )
  }
  
}

df2alpha <- function(df, 
                     split_by = F, #column number to split the plot
                     add_legend = F, #column number(s) to add legend.
                     alpha_function_list = list(
                       "Shannon" = shannon,
                       "Simpson" = simpson),
                     ...) {
  gg <- df %>%
    group_by(sample) %>%
    summarise(across(N, alpha_function_list))
  
  gg <- gg %>% pivot_longer(cols = -1)
  
  gg$name <- gg$name %>% str_remove_all("N_") %>% factor %>% fct_inorder
  
  if(!isFALSE(add_legend)) {
    leg <- unique(df[,c(3,add_legend)])
    leg[,2] <- leg[,-1] %>% apply(1, str_c, collapse = ", ")
    gg <- left_join(leg[,1:2], gg, by = "sample")
    colnames(gg)[2] <- "sample2"
    
  } else {
    leg = unique(df[,3])
    gg$sample2 <- gg$sample
  }
  
  if (!isFALSE(split_by)) {
    
    leg <- unique(df[,c(3,split_by)])
    colnames(leg)[2] <- "split"
    gg <- left_join(leg[,1:2], gg, by = "sample")
    gg$sample2 <- fct_inorder(gg$sample2)
    
    ggplot(gg, aes(y = value, x = split)) +
      geom_boxplot(outlier.alpha = 0,aes(fill = split), alpha = 0.3,...) +
      geom_jitter(aes(color = sample2), width = 0.1,...) +
      facet_wrap(~name, drop = T, ncol = 4, scales = "free", strip.position = "top") +
      scale_color_discrete(NULL, 
                           type = viridis(length(unique(gg$sample2)))) +
      scale_fill_discrete(NULL) +
      ylab("") + xlab ("") +
      theme_minimal() +
      theme(legend.position = "right", 
            #axis.text.y = element_blank(),
            axis.text.x = element_blank())
  } else {
    ggplot(gg, aes(x = value, y = name)) +
      geom_boxplot(outlier.alpha = 0,...) +
      geom_jitter(aes(color = sample2), width = 0.1,...) +
      facet_wrap(~name, drop = T, ncol = 4, scales = "free", strip.position = "top") +
      scale_color_discrete("", 
                           type = viridis(length(unique(gg$sample))), 
                           labels = as.character(unlist(leg[,-1]))) +
      ylab("") + xlab ("") +
      theme_minimal() +
      theme(legend.position = "right", 
            axis.text.y = element_blank(),
            legend.text = element_text())
  }
  
}

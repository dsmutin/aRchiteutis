df2composition <- function(df) {

  lvir <- df$taxa %>% factor %>% levels %>% length
  
  ggplot(df, aes(y = sample, x = amount,
                 fill = fct_inorder(taxa))) +
    geom_col(position = "stack") +
    scale_fill_discrete("Taxa", type = rev(viridis(lvir))) +
    theme_minimal()
}

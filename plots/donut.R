df2donut <- function(df, ...) {
  
  df <- summarise(df, mean(amount), .by = c("taxa","clade"))
  df <- df[order(df$`mean(amount)`),]
  df$ymax = cumsum(df$`mean(amount)`)
  df$ymin = c(0, head(df$ymax, n=-1))
  
  lvir <- df$taxa %>% factor %>% levels %>% length
  lvir <- viridis(lvir)
  
  ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, 
                 fill = fct_inorder(taxa))) +
    geom_rect(color = "white") +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    scale_fill_discrete("", type = lvir) +
    theme_void()
}

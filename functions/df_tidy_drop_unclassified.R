df_tidy_drop_unclassified <- function(df) {
  df <- df[!(df$taxa %>% str_detect("unclassified")),]
}

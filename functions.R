#==========================================================
#FUNCTIONS#
#==========================================================

academic_by_department <- function(df) {
  df %>% filter(Academic == TRUE) %>%
    group_by(Category) %>%
    summarize(avg = mean(Total),
              n = n()) %>%
    mutate(Category = reorder(Category, avg))
}


compare_between_titles.plot <- function(df, filter_df = TRUE) {
  if (filter_df)
  {
    dfn <- df %>% filter(avg > 40000 & n > 2)
  } else {
    dfn <- df
  }
  # fix from https://stackoverflow.com/questions/5106782/use-of-ggplot-within-another-function-in-r
  plot_to_return <- ggplot(dfn, aes(x = Category, y = avg), environment = environment()) + 
    geom_bar(stat = 'identity') + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    # get the first index for which avg > (1,2,3)00k
    geom_vline(xintercept = sapply(c(1, 2, 3) * 1e5, function(x) which(sort(dfn$avg) > x)[1])) +
    geom_text(aes(label = dfn$n), angle = 90, hjust = 1)
  return(plot_to_return)
}
#==========================================================
#FUNCTIONS#
#==========================================================

# this file depends on ggplot, dplyr and must be sourced properly before
# anything will work.

library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(hash)
library(reshape2)
library(scales)

academic_by_department <- function(df) {
  df %>% filter(Academic == TRUE) %>%
    group_by(Category) %>%
    summarize(avg = mean(Total),
              n = n()) %>%
    mutate(Category = reorder(Category, avg))
}

academic_by_department_sub <- function(df) {
  df %>% filter(Academic == TRUE) %>%
    group_by(Category) %>%
    summarize(avg = mean(Base + Overtime),
              n = n()) %>%
    mutate(Category = reorder(Category, avg))
}


compare_between_titles.plot <- function(df, filter_df = TRUE) {
  
  dfn <- if (filter_df) { filter(df, avg > 40000 & n > 2) } else { df }
  
  # fix from
  # https://stackoverflow.com/questions/5106782/use-of-ggplot-within-
  # another-function-in-r
  plot_to_return <- ggplot(dfn, aes(x = Category, y = avg),
                           environment = environment()) + 
    geom_bar(stat = 'identity') + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    # get the first index for which avg > (1,2,3)00k
    geom_vline(xintercept = sapply(c(1, 2, 3) * 1e5,
                                   function(x) which(sort(dfn$avg) > x)[1])) +
    geom_hline(yintercept = c(1, 2, 3) * 1e5) +
    geom_text(aes(label = dfn$n), angle = 90, hjust = 1, color = '#FFFFFF') +
    labs(x = 'CTO Name', y = 'Average Compensation')
  return(plot_to_return)
}


# For 'Workforce Headcount vs. UC Student Enrollment, 2012-2014' Graph

# this function finds the percent increase per year for 2012-14
# takes in a numeric vector with three entries
# will output a vector with three entries for percent increases for 2012-13
# and 2013-14
# first entry will be zero as a baseline and to show increase between 2012-13
# in a plot
find_percent <- function(totals){
  diff = c(totals[2] - totals[1], totals[3] - totals[2])
  percents = diff / totals[1:2]
  percents = c(0, percents)
  return(percents)
}


# STAT 133 FALL 2015 Final Project - analyzing UC payroll data for 2011:2014

library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(hash)
library(reshape2)
library(scales)

# this is where the data resides
source('clean_data.R')

# clean_data.R has now defined uc(2011:2014), along with titles. 
# We will be using these as our source dataframes.


#######################################################
#SORTING BY ACADEMIC AND NON-ACADEMIC#
#######################################################

source('functions.R')
# we load academic_by_department and compare_between_titles.plot
# code reproduced below for clarity

academic_by_department <- function(df) {
  df %>% filter(Academic == TRUE) %>%
    group_by(Category) %>%
    summarize(avg = mean(Total),
              n = n()) %>%
    mutate(Category = reorder(Category, avg))
}

uc2011.by_department <- academic_by_department(uc2011)
uc2012.by_department <- academic_by_department(uc2012)
uc2013.by_department <- academic_by_department(uc2013)
uc2014.by_department <- academic_by_department(uc2014)

# try to run some rouch stats across all three canonical years.
uc12_14 <- rbind(uc2012.by_department, uc2013.by_department,
                 uc2014.by_department) %>%
  group_by(Category) %>% 
    summarize(avg = mean(avg),
              n = floor(mean(n))) %>%
      mutate(Category = reorder(Category, avg))

compare_between_titles.plot <- function(df, filter_df = TRUE) {
  
  dfn <- as.data.frame(ifelse(filter_df, df %>% filter(avg > 40000 & n > 2),
                              df))
  
  # fix from 
  # https://stackoverflow.com/questions/5106782/use-of-ggplot-within-another
  # -function-in-r
  plot_to_return <- ggplot(dfn, aes(x = Category, y = avg),
                           environment = environment()) + 
    geom_bar(stat = 'identity') + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    # get the first index for which avg > (1,2,3)00k
    geom_vline(xintercept = sapply(c(1, 2, 3) * 1e5,
                                   function(x) which(sort(dfn$avg) > x)[1])) +
    geom_text(aes(label = dfn$n), angle = 90, hjust = 1)
  return(plot_to_return)
}

uc2011.by_department.plot <- compare_between_titles.plot(uc2011.by_department,
                                                         FALSE)
uc2012.by_department.plot <- compare_between_titles.plot(uc2012.by_department,
                                                         TRUE)
uc2013.by_department.plot <- compare_between_titles.plot(uc2013.by_department,
                                                         TRUE)
uc2014.by_department.plot <- compare_between_titles.plot(uc2014.by_department,
                                                         TRUE)
uc12_14.plot <- compare_between_titles.plot(uc12_14, TRUE)

across_years <- data.frame(
  'Year' = c(2012, 2013, 2014),
  'Academic' = c(sum(uc2012[which(uc2012$Academic == TRUE), 'Total']),
                 sum(uc2013[which(uc2013$Academic == TRUE), 'Total']),
                 sum(uc2014[which(uc2014$Academic == TRUE), 'Total'])),
  
  'Nonacademic' = c(sum(uc2012[which(uc2012$Academic == FALSE), 'Total']),
                    sum(uc2013[which(uc2013$Academic == FALSE), 'Total']),
                    sum(uc2014[which(uc2014$Academic == FALSE), 'Total']))
)

across_years <- across_years %>% 
  mutate(Difference = Nonacademic - Academic,
         Total = Academic + Nonacademic)


# the code here is different from that in the rmd because sizing, etc.
gp <- ggplot(
  melt(across_years[, c('Nonacademic', 'Academic', 'Year')], 
       id.vars = 'Year', value.name = 'Amount'), 
  aes(x = Year, y = Amount)) + 
  
  geom_bar(aes(fill = variable), stat = 'identity', position = 'fill') + 
  scale_y_continuous(labels = percent_format()) + 
  labs(fill = "", title = 'Academic vs. Nonacademic Spending')

# Acad/Nonacad amounts
gp <- gp + geom_text(aes(label = paste0('$', format(Amount, big.mark = ',')), 
                         y = 0.4, ymax = 1), 
                     position = 'stack', size = 7, color = 'azure',
                     fontface = 'bold')
# total amt
gp <- gp + geom_text(data = across_years, 
                     aes(label = paste0('$', format(Total, big.mark = ',')), 
                         y = 0.2,
                         x = Year), 
                     size = 10, 
                     color = 'white', 
                     fontface = 'bold', 
                     position = 'stack') +
  theme(legend.position = 'bottom', plot.title = element_text(size = rel(2))) 
# amt diff
gp <- gp + geom_text(data = across_years,
                     aes(label = paste0('-$',
                                        format(Difference, big.mark = ',')),
                         y = 0.15,
                         x = Year),
                     position = 'stack',
                     color = 'yellow')

#######################################
#EXPLORATORY ANALYSIS#
#######################################

acad_2012 <- uc2012[uc2012$Academic, ]
non_acad_2012 <- uc2012[!uc2012$Academic, ]

acad_2013 <- uc2013[uc2013$Academic, ]
non_acad_2013 <- uc2013[!uc2013$Academic, ]

acad_2014 <- uc2014[uc2014$Academic, ]
non_acad_2014 <- uc2014[!uc2014$Academic, ]


tenure_prof <- acad_2013$Total[acad_2013$Category == 'PROFESSORIAL-TENURE']
qplot(x = x, geom = 'density',
      main = 'Distribution of Total Pay of Tenured Professors (2013)',
      xlab = 'Total Pay')

acad_2013[which.max(acad_2013$Total), ] #professor making the most money

avg_pay <- tapply(acad_2013$Total, acad_2013$Category, mean)
avg_pay <- sort(avg_pay, decreasing = TRUE)
avg_pay

#smaller groupings: LECT, ADMIN, PROF, GSR/TA/POSTDOC
copy_2013 <- acad_2013
copy_acad$Category[grep('LECTURER', copy_acad$Category)] = 'LECTURER'
copy_acad$Category[grep('DEAN|ACADEMIC|DIRECTOR|PROVOST',
                        copy_acad$Category)] = 'ADMINISTRATOR'
copy_acad$Category[grep('POSTDOCTORAL|INSTRUCTIONAL|GRADUATE|TEACHING',
                        copy_acad$Category)] = 'GSR/TA/POSTDOC'
copy_acad$Category[grep('PROF', copy_acad$Category)] = 'PROFESSOR'
x = tapply(copy_acad$Total, copy_acad$Category, mean)
y = x[c('ADMINISTRATOR', 'LECTURER', 'PROFESSOR', 'GSR/TA/POSTDOC')]
barplot(sort(y))





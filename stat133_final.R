
# STAT 133 FALL 2015 Final Project - analyzing UC payroll data for 2011:2014

library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(hash)
<<<<<<< HEAD
library(reshape2)
=======
>>>>>>> angela
library(scales)

uctca_colnames <- c("Name", "Title", "Base", "Overtime", "Other", "Benefits", "Subtotal", "Total", "Year", "Notes", "Agency")

# my working dir: ~/Dropbox/College/Freshman Year/Fall Semester/STAT 133/project
setwd('~/Dropbox/College/Freshman Year/Fall Semester/STAT 133/project')

# these are really slow. Not recommended to run
#download.file('http://transparentcalifornia.com/export/university-of-california-2011.csv', 'university-of-california-2011.csv')
#download.file('http://transparentcalifornia.com/export/university-of-california-2012.csv', 'university-of-california-2012.csv')
#download.file('http://transparentcalifornia.com/export/university-of-california-2013.csv', 'university-of-california-2013.csv')
#download.file('http://transparentcalifornia.com/export/university-of-california-2014.csv', 'university-of-california-2014.csv')

#restrict Total more? - Michael
uc2011 <- read_csv("university-of-california-2011.csv", col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)
uc2012 <- read_csv("university-of-california-2012.csv", col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)
uc2013 <- read_csv("university-of-california-2013.csv", col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)
uc2014 <- read_csv("university-of-california-2014.csv", col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)

attributes(uc2013)$problems <- NULL # problems pollute the dataset
attributes(uc2014)$problems <- NULL


# 
# ###############################################################
# #This section defines totals by department/category - Michael
# ###############################################################
# 
# #using functions to get total in each department
# 
# title_to_dep <- get_dep(uc2013$Title) # departments, but ungrouped
# n_dep <- apply(title_to_dep, 2, sum) # calculate the number of people in each department, including double-counting
# 
# barplot(n_dep, las = 2, main = 'Total in Each Defined Department, 2013')
# 
# 
# grouped_n_dep <- c('Faculty' = sum(n_dep[c('Instructor', 'Lecturer',
#                            'Associate', 'Adjunct', 'Assistant',
#                            'Academic', 'Visiting', 'Professor')]),
#              'Staff' = sum(n_dep[c('Engineer', 'Technician', 'Accounting',
#                            'Nurse', 'Maintenance')]),
#              'Admin' = sum(n_dep[c('Admissions', 'Administrator')]),
#              'Athletics' = sum(n_dep['Athletics']))
# 
# barplot(grouped_n_dep, main = 'Total by Category, 2013') #not a good representation....too much faculty
# 
# #same thing for 2014
# 
# out14 = get_dep(uc2014$Title)
# tot_dep14 = sum_dep(out14)
# barplot(tot_dep14, las = 2, main = 'Total in Each Defined Department, 2014')
# tot_dep14_2 = c(sum(tot_dep14[c('Instructor', 'Lecturer',
#                            'Associate', 'Adjunct', 'Assistant',
#                            'Academic', 'Visiting', 'Professor')]),
#              sum(tot_dep14[c('Engineer', 'Technician', 'Accounting',
#                            'Nurse', 'Maintenance')]),
#              sum(tot_dep14[c('Admissions', 'Administrator')]),
#              sum(tot_dep14['Athletics']))
# names(tot_dep14_2) = c('Faculty', 'Staff', 'Admin', 'Athletics')
# barplot(tot_dep14_2, main = 'Total by Category, 2014')

#graphs for both years extremely similiar


#######################################################
#SORTING BY ACADEMIC AND NON-ACADEMIC#
#######################################################

titles <- read.csv('academic-titles.csv', strip.white = T, stringsAsFactors = F)
#titles <- titles[-497, c('Title', 'CTO.Name')]

uc2011 <- left_join(uc2011, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))
uc2012 <- left_join(uc2012, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))
uc2013 <- left_join(uc2013, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))
uc2014 <- left_join(uc2014, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))

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
               
uc2013.by_department.filtered <- uc2013.by_department %>%
  filter(avg > 40000 & n > 2)

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

uc2011.by_department.plot <- compare_between_titles.plot(uc2011.by_department, FALSE)
uc2012.by_department.plot <- compare_between_titles.plot(uc2012.by_department, TRUE)
uc2013.by_department.plot <- compare_between_titles.plot(uc2013.by_department, TRUE)
uc2014.by_department.plot <- compare_between_titles.plot(uc2014.by_department, TRUE)

#using functions to get total in each department
# out = get_dep(uc2013$Title)
# tot_dep = sum_dep(out) #in a named list

# barplot(tot_dep, las = 2, main = 'Total in Each Defined Department, 2013')


# tot_dep2 = c(sum(tot_dep[c('Instructor', 'Lecturer',
#                            'Adjunct',
#                            'Academic', 'Visiting', 'Professor')]),
#              sum(tot_dep[c('Engineer', 'Technician', 'Accounting',
#                            'Nurse', 'Maintenance')]),
#              sum(tot_dep[c('Admissions', 'Administrator')]),
#              sum(tot_dep['Athletics']),
#              sum(tot_dep['Student']))
# names(tot_dep2) = c('Faculty', 'Staff', 'Admin', 'Athletics', 'Student')
# barplot(tot_dep2, main = 'Total by Category, 2013') #not a good representation....too much faculty

# #same thing for 2014

# out14 = get_dep(uc2014$Title)
# tot_dep14 = sum_dep(out14)
# barplot(tot_dep14, las = 2, main = 'Total in Each Defined Department, 2014')
# tot_dep14_2 = c(sum(tot_dep14[c('Instructor', 'Lecturer',
#                            'Adjunct',
#                            'Academic', 'Visiting', 'Professor')]),
#              sum(tot_dep14[c('Engineer', 'Technician', 'Accounting',
#                            'Nurse', 'Maintenance')]),
#              sum(tot_dep14[c('Admissions', 'Administrator')]),
#              sum(tot_dep14['Athletics']),
#              sum(tot_dep14['Student']))
# names(tot_dep14_2) = c('Faculty', 'Staff', 'Admin', 'Athletics', 'Student')
# barplot(tot_dep14_2, main = 'Total by Category, 2014')

#graphs for both years extremely similiar

#99,830 persons unaccounted for, or ~41%

#######################################################
#SORTING BY ACADEMIC AND NON-ACADEMIC#
#######################################################




# #######################################
# #EXPLORATORY ANALYSIS#
# #######################################
# tenure_prof = acad_2013$Total[acad_2013$Category == 'PROFESSORIAL-TENURE']
# qplot(x = x, geom = 'density', main = 'Distribution of Total Pay of Tenured Professors (2013)',
#       xlab = 'Total Pay')
# acad_2013[acad_2013$Total == max(acad_2013$Total), ] #professor making the most money

# avg_pay = tapply(acad_2013$Total, acad_2013$Category, mean)
# avg_pay = sort(avg_pay, decreasing = TRUE)
# avg_pay

# #smaller groupings: LECT, ADMIN, PROF, GSR/TA/POSTDOC
# copy = acad_2013
# copy$Category[grep('LECTURER', copy$Category)] = 'LECTURER'
# copy$Category[grep('DEAN|ACADEMIC|DIRECTOR|PROVOST', copy$Category)] = 'ADMINISTRATOR'
# copy$Category[grep('POSTDOCTORAL|INSTRUCTIONAL|GRADUATE|TEACHING', copy$Category)] = 'GSR/TA/POSTDOC'
# copy$Category[grep('PROF', copy$Category)] = 'PROFESSOR'
# x = tapply(copy$Total, copy$Category, mean)
# y = x[c('ADMINISTRATOR', 'LECTURER', 'PROFESSOR', 'GSR/TA/POSTDOC')]
# barplot(sort(y))



# #######################################
# #CAL STATE DATA#
# #######################################

# cal_state <- read.csv('2014-university-system.csv')
# cal_state <- cal_state[cal_state$Agency == 'California State University', ]
# colnames(cal_state) = uctca_colnames
# length(unique(grep('FACULTY|READER|ACADEMIC|LECTURER', cal_state$Title, value = TRUE)))
# #does not use same titles as uc data, can check for similiar subgrouping on internets

# across_years <- data.frame(
#   'Year' = c(2012, 2013, 2014),
#   'Academic' = c(sum(uc2012[which(uc2012$Academic == TRUE), 'Total']),
#                  sum(uc2013[which(uc2013$Academic == TRUE), 'Total']),
#                  sum(uc2014[which(uc2014$Academic == TRUE), 'Total'])),
  
#   'Nonacademic' = c(sum(uc2012[which(uc2012$Academic == FALSE), 'Total']),
#                     sum(uc2013[which(uc2013$Academic == FALSE), 'Total']),
#                     sum(uc2014[which(uc2014$Academic == FALSE), 'Total']))
# )

# across_years <- across_years %>% 
#   mutate(Difference = Nonacademic - Academic,
#          Total = Academic + Nonacademic)

# gp <- ggplot(melt(across_years[, c('Nonacademic', 'Academic', 'Year')], id.vars = 'Year', value.name = 'Amount'), aes(x = Year, y = Amount)) + 
#   geom_bar(aes(fill = variable), stat = 'identity', position = 'fill') + 
#   scale_y_continuous(labels = percent_format()) + labs(fill = "", title = 'Academic vs. Nonacademic Spending')

# # Acad/Nonacad amounts
# gp <- gp + geom_text(aes(label = paste0('$', format(Amount, big.mark = ',')), 
#                          y = 0.4, ymax = 1), 
#                      position = 'stack', size = 7, color = 'azure', fontface = 'bold')
# # total amt
# gp <- gp + geom_text(data = across_years, 
#                      aes(label = paste0('$', format(Total, big.mark = ',')), 
#                          y = 0.2,
#                          x = Year), 
#                      size = 10, 
#                      color = 'white', 
#                      fontface = 'bold', 
#                      position = 'stack') +
#   theme(legend.position = 'bottom', plot.title = element_text(size = rel(2))) 
# # amt diff
# gp <- gp + geom_text(data = across_years,
#                      aes(label = paste0('-$', format(Difference, big.mark = ',')),
#                          y = 0.15,
#                          x = Year),
#                      position = 'stack',
#                      color = 'yellow')
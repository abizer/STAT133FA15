
# STAT 133 FALL 2015 Final Project - analyzing UC payroll data for 2011:2014

library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(hash)
library(scales)
library(reshape2)

uctca_colnames <- c("Name", "Title", "Base", "Overtime", "Other", "Benefits", "Subtotal", "Total", "Year", "Notes", "Agency")

# my working dir: ~/Dropbox/College/Freshman Year/Fall Semester/STAT 133/project
setwd("C:/Users/Harry/stat133/final/project")

#restrict Total more? - Michael
uc2013 <- read_csv("university-of-california-2013.csv", col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)
uc2014 <- read_csv("university-of-california-2014.csv", col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)

attributes(uc2013)$problems <- NULL # problems pollute the dataset
attributes(uc2014)$problems <- NULL

employee_change <- nrow(uc2014) - nrow(uc2013)
#new_employees <- anti_join(uc2014, uc2013, by = 'Name')

#why distinct? this excludes ~80k with apparently same name (which actually seems odd there's so many) - Michael
uc2013.names <- uc2013 %>% select(Name) %>% distinct() %>% mutate(Name = toupper(Name)) %>%  arrange(desc(Name))
uc2014.names <- uc2014 %>% select(Name) %>% distinct() %>% arrange(desc(Name))

cont_employees <- inner_join(uc2013.names, uc2014.names, by = 'Name')
new_employees <- anti_join(uc2013.names, uc2014.names, by = 'Name')

uc2013.titles <- uc2013 %>% select(Title) %>% distinct() 
uc2014.titles <- uc2014 %>% select(Title) %>% distinct() 

unique_titles <- merge(uc2013.titles, uc2014.titles)

uc2013.total_overhead <- (uc2013 %>% summarize(total = sum(Total)))[[1, 1]] # <- sum(uc2013$Total)
uc2014.total_overhead <- (uc2014 %>% summarize(total = sum(Total)))[[1, 1]] # <- sum(uc2014$Total)


if (!exists('ml'))
  source("department_regexes.R")

#changed function to return a data frame - Michael
get_dep <- function(title)
{ # sapply matches regexes against the title, second line collects matched department names
  out = sapply(ml, function(pattern) grepl(pattern, title, perl = TRUE))
  return(data.frame(out))
  #out <- names(out)[out]
  #ifelse(!is.null(out), out, 'Undetermined')
}

dataset_metrics <- function()
{
  match_dep <- function(title)
  {
   # print(title)
    matched_departments <- sapply(ml, function(pattern) grepl(pattern, title, perl = TRUE))
    # return the name of the first matched department, just for metrics on how much of the dataset has been tagged
    #matched <- names(matched_departments)[matched_departments[1]]
    return(
      ifelse(any(matched_departments), T, F)
    )
  }
  
  tagged <- uc2013.employee_stats %>%
    group_by(Title) %>%
    mutate(department = match_dep(Title)) %>%
    select(Title, department)
  
  num_tagged <- sum(tagged$department)
  percent_tagged = num_tagged / nrow(tagged)
  
  return(list(
    percent_tagged = percent_tagged,
    num_untagged = nrow(tagged) - num_tagged,
    untagged = tagged$Title[which(tagged$department == FALSE)]
  ))

}

uc2013.employee_stats <- uc2013 %>% 
  group_by(Title) %>% 
    summarize(n = n(),
              avg_base = mean(Base), 
              avg_benefit = mean(Benefits),
              avg_total = mean(Total),
              group_total = sum(Total))
  
#uc2013.employee_stats <- uc2013.employee_stats %>%
      #group_by(Title) %>%
        #mutate(department = get_dep(Title))
  
departments <- sapply(ml, function(pat) grepl(pat, uc2013.employee_stats$Title))

uc2013.employee_stats.deps <- cbind(uc2013.employee_stats, departments)

uc2013.faculty <- uc2013.employee_stats.deps %>%
  filter(Professor == TRUE | Instructor == TRUE) %>%
    select(Title, n, avg_base, avg_benefit, avg_total, group_total, Professor, Instructor) %>%
      mutate(category = 'Faculty')

uc2013.faculty.professors <- uc2013.faculty %>% 
  filter(Instructor == FALSE) %>%
    select(-Professor, -Instructor) %>%
      mutate(type = 'Professor')

uc2013.faculty.instructors <- uc2013.faculty %>%
  filter(Professor == FALSE) %>%
    select(-Professor, -Instructor) %>%
      mutate(type = 'Instructor')

uc2013.faculty <- uc2013.faculty.professors %>%
  bind_rows(uc2013.faculty.instructors)

profvinst_plot <- ggplot(uc2013.faculty, aes(x = type, y = avg_base)) + geom_boxplot()

uc2013.professors.adjunct <- uc2013.professors %>%
  filter(Adjunct == TRUE)

uc2013.professors.adjunct.only <- uc2013.professors.adjunct %>%
  filter(Associate == FALSE & Assistant == FALSE)

uc2013.professors.associate <- uc2013.professors %>% 
  filter(Associate == TRUE)

uc2013.professors.associate.only <- uc2013.professors.associate %>%
  filter(Adjunct == FALSE & Assistant == FALSE)

uc2013.professors.assistant <- uc2013.professors %>%
  filter(Assistant == TRUE)

uc2013.professors.assistant.only <- uc2013.professors.assistant %>%
  filter(Adjunct == FALSE & Associate == FALSE)

uc2013.professors.full <- uc2013.professors %>%
  filter(Associate == FALSE & Adjunct == FALSE & Assistant == FALSE)
  
filter_department <- function(df, dfdeps, departments) 
{
  df[which(dfdeps[, departments] == TRUE), ]
}

uc2014.employee_stats <- uc2014 %>% 
  group_by(Title) %>% 
    summarize(n = n(), 
              avg_base = mean(Base), 
              avg_benefit = mean(Benefits),
              avg_total = mean(Total),
              group_total = sum(Total)) %>%
      mutate(department = guess_department(Title))



###############################################################
#This section defines totals by department/category - Michael
###############################################################

#using functions to get total in each department

title_to_dep = get_dep(uc2013$Title) # departments, but ungrouped
n_dep = apply(title_to_dep, 2, sum) # calculate the number of people in each department, including double-counting

barplot(n_dep, las = 2, main = 'Total in Each Defined Department, 2013')


grouped_n_dep = c('Faculty' = sum(n_dep[c('Instructor', 'Lecturer',
                           'Associate', 'Adjunct', 'Assistant',
                           'Academic', 'Visiting', 'Professor')]),
             'Staff' = sum(n_dep[c('Engineer', 'Technician', 'Accounting',
                           'Nurse', 'Maintenance')]),
             'Admin' = sum(n_dep[c('Admissions', 'Administrator')]),
             'Athletics' = sum(n_dep['Athletics']))

barplot(grouped_n_dep, main = 'Total by Category, 2013') #not a good representation....too much faculty

#same thing for 2014

out14 = get_dep(uc2014$Title)
tot_dep14 = sum_dep(out14)
barplot(tot_dep14, las = 2, main = 'Total in Each Defined Department, 2014')
tot_dep14_2 = c(sum(tot_dep14[c('Instructor', 'Lecturer',
                           'Associate', 'Adjunct', 'Assistant',
                           'Academic', 'Visiting', 'Professor')]),
             sum(tot_dep14[c('Engineer', 'Technician', 'Accounting',
                           'Nurse', 'Maintenance')]),
             sum(tot_dep14[c('Admissions', 'Administrator')]),
             sum(tot_dep14['Athletics']))
names(tot_dep14_2) = c('Faculty', 'Staff', 'Admin', 'Athletics')
barplot(tot_dep14_2, main = 'Total by Category, 2014')

#graphs for both years extremely similiar


#######################################################

dupli_14 = uc2014$Title
for (i in 1:length(dupli_14)){
  if (dupli_14[i] %in% titles$Title){
    dupli_14[i] = as.character(titles[titles$Title == dupli_14[i], 2])
  }
  else (dupli_14[i] = 'NA')
}

dupli2_14 = uc2014
dupli2_14$Category = dupli_14

na_indexes = grep('\\bNA\\b', dupli_14)
non_acad_2014 = uc2014[na_indexes, ]
non_acad_2014$Category = 'NON-ACADEMIC' #so both dfs have same # of columns
acad_2014 = dupli2_14[-na_indexes, ]



dupli_12 = uc2012$Title
for (i in 1:length(dupli_12)){
  if (dupli_12[i] %in% titles$Title){
    dupli_12[i] = as.character(titles[titles$Title == dupli_12[i], 2])
  }
  else (dupli_12[i] = 'NA')
}

dupli2_12 = uc2012
dupli2_12$Category = dupli_12

na_indexes = grep('\\bNA\\b', dupli_12)
non_acad_2012 = uc2012[na_indexes, ]
non_acad_2012$Category = 'NON-ACADEMIC' #so both dfs have same # of columns
acad_2012 = dupli2_12[-na_indexes, ]




#2011 data uses different job titles (e.g. PROFESSOR-HCOMP instead of PROF-HCOMP) so can't use
dupli_11 = uc2011$Title
for (i in 1:length(dupli_11)){
  if (dupli_11[i] %in% titles$Title){
    dupli_11[i] = as.character(titles[titles$Title == dupli_11[i], 2])
  }
  else (dupli_11[i] = 'NA')
}

dupli2_11 = uc2011
dupli2_11$Category = dupli_11

na_indexes = grep('\\bNA\\b', dupli_11)
non_acad_2011 = uc2011[na_indexes, ]
non_acad_2011$Category = 'NON-ACADEMIC' #so both dfs have same # of columns
acad_2011 = dupli2_11[-na_indexes, ]




t_acad = c(sum(acad_2012$Total), sum(acad_2013$Total), sum(acad_2014$Total))
t_non = c(sum(non_acad_2012$Total), sum(non_acad_2013$Total), sum(non_acad_2014$Total))
df_tot = data.frame(year = c(2012, 2013, 2014), acad = t_acad, non = t_non)
df_tot$dif = df_tot$non - df_tot$acad

#Total Overhead over Time
ggplot(data = df_tot, aes(year)) +
  geom_point(aes(y = acad, color = 'acad')) +
  geom_point(aes(y = non, color = 'non')) +
  geom_line(aes(y = acad, color = 'acad')) +
  geom_line(aes(y = non, color = 'non')) +
  ggtitle('Total Overhead by Academic and Non-Academic (2012-14)') +
  xlab('Year') +
  ylab('Total') + scale_y_continuous(labels = comma)


#Harry's Code#
#~ ~ ~
#Barplot of Total Overhead
df_no_diff <- df_tot %>% select(-dif)

melted_df_tot <- melt(df_no_diff, id.vars = 'year')

ggplot(data = melted_df_tot, aes(x = factor(year), y = value, fill = factor(variable))) +
  geom_bar(position = "stack", stat = 'identity') +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=c("#007d32", "#b6250b")) +
  xlab('Year') + ylab('Total') + ggtitle ('Total Overhead by Academic and Non-Academic (2012-2014)')
  
#Barplot of Difference in Overhead
ggplot(data = df_tot, aes(x = factor(year), y = dif)) +
  geom_bar(stat = 'identity', fill = '#b6250b') +
  scale_y_continuous(labels = comma) +
  xlab('Year') + ylab('Total') + ggtitle('Difference in Overhead: Non-academic - Academic')+
  geom_text(aes(label = paste0(signif(df_perc$enroll, 5)*100,'%'), y = 2), vjust=-8, size = 14, color = "#FFFFFF")
#~ ~ ~

#Difference in Overhead
ggplot(data = df_tot, aes(year)) +
  geom_point(aes(y = dif, color = 'dif')) +
  geom_line(aes(y = dif, color = 'dif')) +
  ggtitle('Difference between Academic and Non-Academic Total Overhead (2012-14)') +
  xlab('Year') +
  ylab('Difference')

#Difference in 

#Two graphs above combined
ggplot(data = df_tot, aes(year)) +
  geom_point(aes(y = acad, color = 'acad')) +
  geom_point(aes(y = non, color = 'non')) +
  geom_point(aes(y = dif, color = 'dif')) +
  geom_line(aes(y = acad, color = 'acad')) +
  geom_line(aes(y = non, color = 'non')) +
  geom_line(aes(y = dif, color = 'dif')) +
  ggtitle('Total Overhead by Academic and Non-Academic (2012-14)') +
  xlab('Year') +
  ylab('Total/Difference (in USD)')


#Also graphing difference increase in undergraduate enrollment
#Will have to do this as percentages
#source: http://finreports.universityofcalifornia.edu/index.php?file=13-14/pdf/fullreport-1314.pdf
enroll = c(179581, 184562, 183498, 191369)
names(enroll) = c('2011', '2012', '2013', '2014')
diff_enroll = c(184562-179581, 183498-184562, 191369-183498)
names(diff_enroll) = c('2011-12', '2012-13', '2013-14')
percent_enroll = diff_enroll[2:3] / enroll[2:3]
percent_enroll = c(0, percent_enroll)

diff_acad = c(nrow(acad_2013)-nrow(acad_2012), nrow(acad_2014)-nrow(acad_2013))
names(diff_acad) = c('2012-13', '2013-14')
percent_acad = diff_acad / c(nrow(acad_2012), nrow(acad_2013))
percent_acad = c(0, percent_acad)

diff_non = c(nrow(non_acad_2013)-nrow(non_acad_2012), nrow(non_acad_2014)-nrow(non_acad_2013))
names(diff_non) = c('2012-13', '2013-14')
percent_non = diff_non / c(nrow(non_acad_2012), nrow(non_acad_2013))
percent_non = c(0, percent_non)

df_perc = data.frame(enroll = percent_enroll, acad = percent_acad, 
                     non = percent_non, total = percent_acad+percent_non, 
                     year = c(2012:2014), row.names = NULL)

ggplot(data = df_perc, aes(year)) +
  geom_point(aes(y = acad, color = 'acad')) +
  geom_point(aes(y = non, color = 'non')) +
  geom_point(aes(y = total, color = 'total')) +
  geom_point(aes(y = enroll, color = 'enroll')) +
  geom_line(aes(y = acad, color = 'acad')) +
  geom_line(aes(y = non, color = 'non')) +
  geom_line(aes(y = total, color = 'total')) +
  geom_line(aes(y = enroll, color = 'enroll')) +
  ggtitle('Percent Increase in Enrollment Compared to Faculty/Staff (2012-14)') +
  xlab('Year') +
  ylab('Percent')

#######################################
#EXPLORATORY ANALYSIS#
#######################################
tenure_prof = acad_2013$Total[acad_2013$Category == 'PROFESSORIAL-TENURE']
qplot(x = x, geom = 'density', main = 'Distribution of Total Pay of Tenured Professors (2013)',
      xlab = 'Total Pay')
acad_2013[acad_2013$Total == max(acad_2013$Total), ] #professor making the most money

avg_pay = tapply(acad_2013$Total, acad_2013$Category, mean)
avg_pay = sort(avg_pay, decreasing = TRUE)
avg_pay

#smaller groupings: LECT, ADMIN, PROF, GSR/TA/POSTDOC
copy = acad_2013
copy$Category[grep('LECTURER', copy$Category)] = 'LECTURER'
copy$Category[grep('DEAN|ACADEMIC|DIRECTOR|PROVOST', copy$Category)] = 'ADMINISTRATOR'
copy$Category[grep('POSTDOCTORAL|INSTRUCTIONAL|GRADUATE|TEACHING', copy$Category)] = 'GSR/TA/POSTDOC'
copy$Category[grep('PROF', copy$Category)] = 'PROFESSOR'
x = tapply(copy$Total, copy$Category, mean)
y = x[c('ADMINISTRATOR', 'LECTURER', 'PROFESSOR', 'GSR/TA/POSTDOC')]
barplot(sort(y))



#######################################
#CAL STATE DATA#
#######################################

cal_state <- read.csv('2014-university-system.csv')
cal_state <- cal_state[cal_state$Agency == 'California State University', ]
colnames(cal_state) = uctca_colnames
length(unique(grep('FACULTY|READER|ACADEMIC|LECTURER', cal_state$Title, value = TRUE)))
#does not use same titles as uc data, can check for similiar subgrouping on internets

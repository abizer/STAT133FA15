
# STAT 133 FALL 2015 Final Project - analyzing UC payroll data for 2011:2014

library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(hash)

uctca_colnames <- c("Name", "Title", "Base", "Overtime", "Other", "Benefits", "Subtotal", "Total", "Year", "Notes", "Agency")

# my working dir: ~/Dropbox/College/Freshman Year/Fall Semester/STAT 133/project
setwd('~/Dropbox/College/Freshman Year/Fall Semester/STAT 133/project')

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
#SORTING BY ACADEMIC AND NON-ACADEMIC#
#######################################################


titles = read_csv('academic-ttles-sorted-title-name.csv')
titles = titles[-497, c('Title', 'CTO Name')]
#titles = titles[-497, ]
#titles$Title = as.character(titles$Title)

#fixing some problems
#problems found by grep-ing 'PROF', 'GSHIP', etc. in non_acad
titles$Title[grep('PROF OF CLIN- FY', titles$Title)] = 'PROF OF CLIN-FY'
titles$Title[grep("RES PROF-MILLER INST -AY", titles$Title)] = "RES PROF-MILLER INST-AY" 
titles$Title[grep("PROF EMERITUS \\(WOS\\)", titles$Title)] = "PROF EMERITUS(WOS)" 
titles$Title[grep("LECT SOE-EMERITUS \\(WOS\\)", titles$Title)] = "LECT SOE-EMERITUS(WOS)" 

titles$Title[grep("ASSOC IN            - AY-1/9-GSHIP" , titles$Title)] = "ASSOC IN __ -AY-1/9-GSHIP"
titles$Title[grep("ASSOC IN            -AY-1/9-NON-GSHIP", titles$Title)] = "ASSOC IN__-AY- 1/9 -NON-GSHIP"
titles$Title[grep("ASSOC IN            -AY-GSHIP" , titles$Title)] = "ASSOC IN ____-AY-GSHIP" 
titles$Title[grep("READER - GSHIP", titles$Title)] = "READER-GSHIP" 
titles$Title[grep("REMD TUT I-NON-GSHIP/NON REP", titles$Title)] = "REMD TUT I-NON GSHIP/NON REP"
titles$Title[grep("REMD TUT I-NON-GSHIP", titles$Title)] = "REMD TUT I-NON GSHIP"
titles$Title[grep("READER - NON GSHIP", titles$Title)] = "READER-NON GSHIP"
titles$Title[grep("TUT--NON STDNT/NON REP", titles$Title)] = "TUT-NON STDNT/NON REP"
titles$Title[grep("READER - NON STDNT", titles$Title)] = "READER-NON STDNT"


unique_titles <- unique(uc2013$Title)
title_map <- list()

#this for loop takes a while to run, probably a more efficient way
#getting warnings for this, didn't before, could be line 135 (as.character...)?
#seems to be working fine though
for (i in unique_titles) {
  title_map[[i]] <- as.character(titles[titles$Title == i, 'CTO Name'])
}

title_map <- sapply(title_map, function(x) ifelse(x == "character(0)", NA, x))

#title_df <- data.frame(Title = names(title_map), Department = title_map, row.names = NULL, stringsAsFactors = FALSE)

uc2013.departments <- uc2013 %>% mutate(Department = sapply(Title, function(x) title_map[[x]]))

uc2013.by_department <- uc2013.departments %>% 
  group_by(Department) %>% 
    summarize(avg = mean(Total)) %>% 
     # arrange(avg)
        filter(avg > 40000 & !is.na(Department)) %>%
      mutate(Department = reorder(Department, avg))

group_comp_all_plot <- ggplot(uc2013.by_department, aes(x = Department, y = avg)) + 
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

group_comp_all_plot + geom_vline(aes(xintercept = sapply(c(1, 2, 3) * 1e5, function(x) (which(sort(uc2013.by_department$avg) > x)[1]))), 
                                 linetype = 1, color = 'black')
                                                             

#need to make a duplicate to preserve grouped academic titles
dupli2 = uc2013
dupli2$Category = dupli

na_indexes = grep('\\bNA\\b', dupli)
non_acad_2013 = uc2013[na_indexes, ]
non_acad_2013$Category = 'NON-ACADEMIC' #so both dfs have same # of columns
acad_2013 = dupli2[-na_indexes, ]

#acad_2013 excludes visiting and recalled professors/lecturers, also 'RESEARCH PROFESSOR'
#and 'PROFESSOR-FY-GENCOMP'
#left 'SPECIAL READER...UCLA' in non_acad
#'COORD', 'TEACHER' series also messed up
#should include 'DEAN' stuffs too, 307 in non_acad; also LIBRARIAN



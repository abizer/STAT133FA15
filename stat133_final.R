
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
uc2011 <- read_csv("university-of-california-2011.csv", col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)
uc2012 <- read_csv("university-of-california-2012.csv", col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)
uc2013 <- read_csv("university-of-california-2013.csv", col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)
uc2014 <- read_csv("university-of-california-2014.csv", col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)

attributes(uc2011)$problems <- NULL
attributes(uc2012)$problems <- NULL
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

#added lecturer, students, updated admin to include dean - Michael
ml <- hash()
ml[['Athletics']] <- '^ATH|COACH'

ml[['Administrator']] <- 'SUPV|MGR|ADMIN(ISTRATOR)?|DEAN'
ml[['Academic']] <- 'ACAD(EMIC)?'
ml[['Maintenance']] <- 'MAINT|TECH'
ml[['Accounting']] <- 'ACCOUNT(ANT|ING)'
#ml[['Assistant']] <- 'ASST' #I think this one should be deleted, lots of assistants for different things - Michael
#ml[['Associate']] <- 'ASSOC' #might also have same problem as above (e.g. assoc dean)
#ml[['Visiting']] <- 'VIS'
#ml[['Adjunct']] <- 'ADJ'

#ml[['Instructor']] <- 'INSTR[^U]' # don't want instrument or instruction supervisor
ml[['Nurse']] <- 'NURSE'
ml[['Admissions']] <- 'ADMISSIONS'
ml[['Engineer']] <- 'ENGR'
ml[['Technician']] <- 'TCHN'
fr[['Student']] <- 'STDT'

fr <- hash()

fr[['Post Doc']] <- 'POSTDOC'
fr[['Lecturer']] <- 'LECT[ -]'
fr[['Adjunct']] <- 'ADJ (PROF|INSTR)'
fr[['Assistant']] <- 'ASST PROF'
fr[['Associate']] <- 'ASSOC PROF'
fr[['Professor']] <- 'PROF[^L]'

faculty_regex = sapply(fr, function(pattern) grepl(pattern, uc2013$Title))
faculty = data.frame(faculty_regex)
for (i in 1:(length(colnames(faculty))-1)){
  faculty[ , i] = sum(faculty[ , i])
}
faculty$Professor = sum(faculty$Professor) - sum(faculty[1, c(1, 3, 4)])
faculty = faculty[1, ]
faculty_num = as.numeric(faculty)
names(faculty_num) = colnames(faculty)
barplot(faculty_num, las = 2, main = 'Faculty/Instructor Breakdown 2013', 
        xlab = 'Type of Professor', ylab = 'Total', cex.axis = 0.7)

#faculty_gg = data.frame(type = colnames(faculty), total = faculty_num, row.names = NULL)
#ggplot(data = faculty) +
  #geom_bar() + 
  #xlab('Type of Professor') +
  #ylab('Total') + 
  #ggtitle('Faculty/Instructor Breakdown 2013')


#faculty = Filter(function(x) (any(x)), faculty_regex)

#changed function to return a data frame - Michael
get_dep <- function(title)
{ # sapply matches regexes against the title, second line collects matched department names
  out = sapply(ml, function(pattern) grepl(pattern, title))
  return(data.frame(out))
  #out <- names(out)[out]
  #ifelse(!is.null(out), out, 'Undetermined')
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

#getting error for uc2013.professors
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

#this function calculates the total in each department
vec = c()
sum_dep <- function(out){
  for (i in 1:length(colnames(out))){
    sum = sum(out[ , i])
    vec = c(vec, sum)
  }
  names(vec) = colnames(out)
  return(vec)
}

#using functions to get total in each department
out = get_dep(uc2013$Title)
tot_dep = sum_dep(out) #in a named list

barplot(tot_dep, las = 2, main = 'Total in Each Defined Department, 2013')


tot_dep2 = c(sum(tot_dep[c('Instructor', 'Lecturer',
                           'Adjunct',
                           'Academic', 'Visiting', 'Professor')]),
             sum(tot_dep[c('Engineer', 'Technician', 'Accounting',
                           'Nurse', 'Maintenance')]),
             sum(tot_dep[c('Admissions', 'Administrator')]),
             sum(tot_dep['Athletics']),
             sum(tot_dep['Student']))
names(tot_dep2) = c('Faculty', 'Staff', 'Admin', 'Athletics', 'Student')
barplot(tot_dep2, main = 'Total by Category, 2013') #not a good representation....too much faculty

#same thing for 2014

out14 = get_dep(uc2014$Title)
tot_dep14 = sum_dep(out14)
barplot(tot_dep14, las = 2, main = 'Total in Each Defined Department, 2014')
tot_dep14_2 = c(sum(tot_dep14[c('Instructor', 'Lecturer',
                           'Adjunct',
                           'Academic', 'Visiting', 'Professor')]),
             sum(tot_dep14[c('Engineer', 'Technician', 'Accounting',
                           'Nurse', 'Maintenance')]),
             sum(tot_dep14[c('Admissions', 'Administrator')]),
             sum(tot_dep14['Athletics']),
             sum(tot_dep14['Student']))
names(tot_dep14_2) = c('Faculty', 'Staff', 'Admin', 'Athletics', 'Student')
barplot(tot_dep14_2, main = 'Total by Category, 2014')

#graphs for both years extremely similiar

#99,830 persons unaccounted for, or ~41%

#######################################################
#SORTING BY ACADEMIC AND NON-ACADEMIC#
#######################################################


titles = read.csv('academic-ttles-sorted-title-name.csv')
titles = titles[ , c(1, 4)]
titles = titles[1:496, ]
titles$Title = as.character(titles$Title)

#fixing some problems
#problems found by grep-ing 'PROF', 'GSHIP', etc. in non_acad
#problems usually from spaces and dashes
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

dupli = uc2013$Title
#this for loop takes a while to run, probably a more efficient way
#getting warnings for this, didn't before, could be line 135 (as.character...)?
#seems to be working fine though
for (i in 1:length(dupli)){
  if (dupli[i] %in% titles$Title){
    dupli[i] = as.character(titles[titles$Title == dupli[i], 2])
  }
  else (dupli[i] = 'NA')
}

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


###########SAME THING FOR 2011, 2012, 2014##############
download.file('http://transparentcalifornia.com/export/university-of-california-2012.csv', 'university-of-california-2012.csv')
download.file('http://transparentcalifornia.com/export/university-of-california-2011.csv', 'university-of-california-2011.csv')



#######################################
#CAL STATE DATA#
#######################################

cal_state <- read.csv('2014-university-system.csv')
cal_state <- cal_state[cal_state$Agency == 'California State University', ]
colnames(cal_state) = uctca_colnames
length(unique(grep('FACULTY|READER|ACADEMIC|LECTURER', cal_state$Title, value = TRUE)))
#does not use same titles as uc data, can check for similiar subgrouping on internets


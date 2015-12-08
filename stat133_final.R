
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

titles = read.csv('academic-titles.csv', stringsAsFactors = FALSE)
#titles = read.csv('academic-ttles-sorted-title-name.csv')
#titles = titles[ , c(1, 4)]
#titles = titles[1:496, ]
#titles$Title = as.character(titles$Title)

#fixing some problems
#problems found by grep-ing 'PROF', 'GSHIP', etc. in non_acad
#problems usually from spaces and dashes
titles$Title[grep('PROF OF CLIN- FY', titles$Title)] = 'PROF OF CLIN-FY'
titles$Title[grep("RES PROF-MILLER INST -AY", titles$Title)] = "RES PROF-MILLER INST-AY" 
titles$Title[grep("PROF EMERITUS \\(WOS\\)", titles$Title)] = "PROF EMERITUS(WOS)" 
titles$Title[grep("LECT SOE-EMERITUS \\(WOS\\)", titles$Title)] = "LECT SOE-EMERITUS(WOS)"

#forgot what I'm doing with this
#titles$Title[grep('PROF.*RECALL', titles$Title)] = "RECALLED PROFESSOR"
#titles$Title[grep('VST.*PROF|VSTG.*PROF', titles$Title)] = "VISTING PROFESSOR"
#titles$Title[grep('RESEARCH PROFESSOR', titles$Title)] = "PROFESSIONAL RESEARCH-REGULAR"

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
#dupli2$Category = dupli

dupli2 <- left_join(uc2013, titles, by = 'Title')
dupli3 = left_join(uc2012, titles, by = 'Title')
dupli4 = left_join(uc2014, titles, by = 'Title')

dupli2$Category = dupli2$CTO.Name
dupli2$CTO.Name = NULL

dupli3$Category = dupli3$CTO.Name
dupli3$CTO.Name = NULL

dupli4$Category = dupli4$CTO.Name
dupli4$CTO.Name = NULL

acad_2012 = dupli3[is.na(dupli3$Category) == FALSE, ]
non_acad_2012 = dupli3[is.na(dupli3$Category) == TRUE, ]
acad_2013 = dupli2[is.na(dupli2$Category) == FALSE, ]
non_acad_2013 = dupli2[is.na(dupli2$Category) == TRUE, ]
acad_2014 = dupli4[is.na(dupli4$Category) == FALSE, ]
non_acad_2014 = dupli4[is.na(dupli4$Category) == TRUE, ]



na_indexes = grep('\\bNA\\b', dupli)
non_acad_2013 = uc2013[na_indexes, ]
non_acad_2013$Category = 'NON-ACADEMIC' #so both dfs have same # of columns
acad_2013 = dupli2[-na_indexes, ]

#left 'SPECIAL READER...UCLA' in non_acad
#'COORD', 'TEACHER' series also messed up
#should include 'DEAN' stuffs too, 307 in non_acad; also LIBRARIAN


###########SAME THING FOR 2011, 2012, 2014##############
download.file('http://transparentcalifornia.com/export/university-of-california-2012.csv', 'university-of-california-2012.csv')
download.file('http://transparentcalifornia.com/export/university-of-california-2011.csv', 'university-of-california-2011.csv')

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


################################################
#GRAPHS#
################################################


#Total Overhead over Time
ggplot(data = df_tot, aes(year)) +
  geom_point(aes(y = acad, color = 'acad')) +
  geom_point(aes(y = non, color = 'non')) +
  geom_line(aes(y = acad, color = 'acad')) +
  geom_line(aes(y = non, color = 'non')) +
  ggtitle('Total Overhead by Academic and Non-Academic (2012-14)') +
  xlab('Year') +
  ylab('Total')
  
#Difference in Overhead
ggplot(data = df_tot, aes(year)) +
  geom_point(aes(y = dif, color = 'dif')) +
  geom_line(aes(y = dif, color = 'dif')) +
  ggtitle('Difference between Academic and Non-Academic Total Overhead (2012-14)') +
  xlab('Year') +
  ylab('Difference')

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


#graphing percent increase in enrollment vs. # of faculty/staff/admin
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


tenure_prof = acad_2014$Total[acad_2014$Category == 'PROFESSORIAL-TENURE']
clin_series = acad_2014$Total[acad_2014$Category == "PROFESSOR OF CLINICAL"]
health_sciences = acad_2014$Total[acad_2014$Category == 'HEALTH SCIENCES CLINICAL PROFESSOR']
dft = data.frame(tenure = tenure_prof)
dft = melt(dft)
dfc = data.frame(clinical = clin_series)
dfc = melt(dfc)
dfh = data.frame(health = health_sciences)
dfh = melt(dfh)
#df = data.frame(tenure = tenure_prof, clinical = clin_series, health_sciences = health_sciences)
qplot(x = tenure_prof, geom = 'density', main = 'Distribution of Total Pay of Tenured Professors (2013)',
      xlab = 'Total Pay')

ggplot() +
  geom_density(data = dft, aes(x = tenure, fill = 'tenure'), alpha = 0.7) + 
  geom_density(data = dfc, aes(x = clinical, fill = 'clinical'), alpha = 0.7) +
  geom_density(data = dfh, aes(x = health, fill = 'health'), alpha = 0.7) +
  scale_fill_discrete(name = 'Legend', labels = c('Clinical', 'Health Sciences Clinical', 'Tenured')) +
  ggtitle('Densities of Total Pay by Type of Professor') +
  xlab('Total') +
  ylab('Density')



#######################################
#EXPLORATORY ANALYSIS#
#######################################
tenure_prof = acad_2013$Total[acad_2013$Category == 'PROFESSORIAL-TENURE']
qplot(x = tenure_prof, geom = 'density', main = 'Distribution of Total Pay of Tenured Professors (2013)',
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


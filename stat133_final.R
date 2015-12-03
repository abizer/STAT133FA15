
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

#added lecturer, students, updated admin to include dean - Michael
ml <- hash()
ml[['Athletics']] <- '^ATH|COACH'
ml[['Professor']] <- 'PROF[^L]'
ml[['Administrator']] <- 'SUPV|MGR|ADMIN(ISTRATOR)?| ADM '
ml[['Academic']] <- 'ACAD(EMIC)?'
ml[['Maintenance']] <- 'MAINT|TECH|MECH'
ml[['Accounting']] <- 'ACCOUNT(ANT|ING)'
ml[['Assistant']] <- 'ASST' #I think this one should be deleted, lots of assistants for different things - Michael
ml[['Associate']] <- 'ASSOC' #might also have same problem as above (e.g. assoc dean)
ml[['Visiting']] <- 'VIS'
ml[['Adjunct']] <- 'ADJ'
ml[['Instructor']] <- 'INSTR[^U]' # don't want instrument or instruction supervisor
ml[['Nurse']] <- 'NURSE'
ml[['Admissions']] <- 'ADMISSIONS'
ml[['Engineer']] <- 'ENGR'
ml[['Technician']] <- 'TCHN'
ml[['Student']] <- 'STDN?T'

ml[['Lecturer']] <- '[^EL]LECT[ -]'
ml[['Adjunct Professor']] <- 'ADJ PROF'
ml[['Assistant Professor']] <- 'ASST PROF'
ml[['Associate Professor']] <- 'ASSOC PROF'
ml[['Assistant Adjunct Professor']] <- 'ASST ADJ PROF'
ml[['Associate Adjunct Professor']] <- 'ASSOC ADJ PROF'
ml[['Professor']] <- 'PROF[^L]'
ml[['Post Doc']] <- 'POSTDOC'

ml[['Vice President']] <- '^S?VP '
ml[['Vice Chancellor']] <- '^VC '
ml[['Researcher']] <- 'RSCH'
ml[['Veterinarian']] <- 'VETERINARIAN'
ml[['Professional']] <- 'PROFL'
ml[['Teacher']] <- 'TEACHER|EDUCATOR'
ml[['Dean']] <- 'DEAN'
ml[['Officer']] <- 'OFCR'
ml[['Marine']] <- '(ABLE SEAMAN)|(SEA CAPTAIN)|^MARINE'
ml[['Analyst']] <- 'ANL' # space intentional
ml[['Therapist']] <- ' THER '
ml[['Consultant']] <- 'CNSLT'
ml[['Counselor']] <- 'CNSLR'
ml[['Medical Misc.']] <- '^MED |^HOSP |^PATIENT |^OPERATING |HEALTH'
ml[['Library Misc.']] <- 'LIBRAR.+'
ml[['Operator']] <- 'OPR'
ml[['Technologist']] <- 'TCHL'
ml[['Physician | PA']] <- ' PHYS(CN)?'
ml[['Medical Resident']] <- '^RES-'
ml[['Programmer']] <- 'PROGR [0-9]'
ml[['Investments']] <- ' INV |REAL ESTATE'
ml[['Media/Marketing']] <- 'MEDIA|MARKETING'
ml[['Legal']] <- 'LEGAL'
ml[['Lab Worker']] <- '^LAB'
ml[['Management']] <- 'MGT'
ml[['Clinial']] <- 'CLIN'
ml[['Police']] <- 'POLICE'
ml[['Random']] <- paste0('MAIL|MASSAGE|MASON|FARM |MASON|LINEN|OILER|',
                         'WIPER|USHER|INSULATION|HIGH VOLT|FUNDRAISER|',
                         'BLANK |BAKER|BUYER|ARTIST|ART MODEL|AGRON')




#changed function to return a data frame - Michael
get_dep <- function(title)
{ # sapply matches regexes against the title, second line collects matched department names
  out = sapply(ml, function(pattern) grepl(pattern, title))
  return(data.frame(out))
  #out <- names(out)[out]
  #ifelse(!is.null(out), out, 'Undetermined')
}

dataset_metrics <- function()
{
  match_dep <- function(title)
  {
   # print(title)
    matched_departments <- sapply(ml, function(pattern) grepl(pattern, title))
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



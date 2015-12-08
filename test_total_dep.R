get_dep <- function(title)
{ # sapply matches regexes against the title, second line collects matched department names
  out = sapply(ml, function(pattern) grepl(pattern, title))
  return(data.frame(out))
  #michael - below lines not working for me
  #out <- names(out)[out]
  #ifelse(!is.null(out), out, 'Undetermined')
}

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

barplot(tot_dep, las = 2, main = 'Total in Each Defined Department')


tot_dep2 = c(sum(tot_dep[c('Instructor', 'Lecturer',
                           'Associate', 'Adjunct', 'Assistant',
                           'Academic', 'Visiting', 'Professor')]),
             sum(tot_dep[c('Engineer', 'Technician', 'Accounting',
                           'Nurse', 'Maintenance')]),
             sum(tot_dep[c('Admissions', 'Administrator')]),
             sum(tot_dep['Athletics']))
names(tot_dep2) = c('Faculty', 'Staff', 'Admin', 'Athletics')
barplot(tot_dep2, main = 'Total by Category') #clearly not a good representation, too much faculty



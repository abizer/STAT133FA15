
# STAT 133 FALL 2015 Final Project - analyzing UC payroll data for 2011:2014

library(readr)
library(ggplot2)
library(dplyr)

uctca_colnames <- c("Name", "Title", "Base", "Overtime", "Other", "Benefits", "Subtotal", "Total", "Year", "Notes", "Agency")

uc2013 <- read_csv("university-of-california-2013.csv", col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)
uc2014 <- read_csv("university-of-california-2014.csv", col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)

employee_change = nrow(uc2014) - nrow(uc2013)
new_employees = anti_join(uc2014, uc2013, by = 'Name')

titles2013 <- uc2013 %>% select(Title) %>% distinct() 
titles2014 <- uc2014 %>% select(Title) %>% distinct() 

unique_titles <- merge(titles2013, titles2014)

names13 <- uc2013 %>% select(Name) %>% distinct()
names14 <- uc2014 %>% select(Name) %>% distinct()


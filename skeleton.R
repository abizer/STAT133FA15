#=======================================================================================
#SKELETON
#=======================================================================================

library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(hash)
library(reshape2)
library(scales)

# creating subdirectories
dir.create('code')
dir.create('rawdata')
dir.create('data')
dir.create('resources')
dir.create('report')
dir.create('images')


# Raw Data
# four years worth of financial data from Transparent California
download.file(paste0('http://transparentcalifornia.com/export/university-',
                     'of-california-2011.csv'),
              'rawdata/university_of_california_2011.csv')
download.file(paste0('http://transparentcalifornia.com/export/university-',
                     'of-california-2012.csv'),
              'rawdata/university_of_california_2012.csv')
download.file(paste0('http://transparentcalifornia.com/export/university-',
                     'of-california-2013.csv'),
              'rawdata/university_of_california_2013.csv')
download.file(paste0('http://transparentcalifornia.com/export/university-',
                     'of-california-2014.csv'),
              'rawdata/university_of_california_2014.csv')


# Resources
# the original titles PDF
download.file(paste0('http://www.ucop.edu/academic-personnel-programs/_files/',
              'acad-title-codes/academic-ttles-sorted-title-name.pdf'),
              'resources/acad_title_codes.pdf')

# UC student enrollment data
# below line shows original source but it will return an error as there is an
# initial page where you have to accept an agreement to view the document
# download.file('http://finreports.universityofcalifornia.edu/
# index.php?file=13-14/pdf/fullreport-1314.pdf',
# 'resources/enrollment-data.pdf')


# creating data dictionary in rawdata subdirectory

uc2014.meta <- read.csv('rawdata/university_of_california_2014.csv')

var.names <- colnames(uc2014.meta)

var.def <- c('Names of each individual',
            'Individual\'s title in the UC database',
            'Base Pay',
            'Overtime Pay',
            'Other Pay',
            'Benefits',
            'Total not including benefits',
            'Total including benefits',
            'Year',
            'Notes - details if an individual holds multiple positions',
            'Agency - all University of California employees')

var.storage <- sapply(uc2014.meta, class)

var.units <- c(NA, NA, 'USD', 'USD', 'USD', 'USD', 'USD', 'USD', 'Date',
               NA, NA)

meta = data.frame(Variable = as.character(var.names),
                  Definition = as.character(var.def),
                  Storage = as.character(var.storage),
                  Units = as.character(var.units),
                  row.names = NULL)

write.csv(meta, 'rawdata/meta_data.csv', quote = F, row.names = F)




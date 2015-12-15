#========================================================
#DATA CLEANING
#========================================================

###########################################################
#FINDING AND SOLVING FOR PROBLEMS IN TITLES
###########################################################

# excluding recalled and visiting professors from dataset
# problems found by grep-ing 'PROF', 'GSHIP', etc. in non_acad
# problems usually from spaces and dashes

# file 'academic-titles.csv' generated from pdf 'acad-title-codes.pdf'
# csv file generated manually via Excel generating function of Adobe Acrobat
# from Excel, this was saved as a .csv file
# manually done because no available dataset not in pdf format; only way to get data in .csv format

if (!file.exists('../data/academic-titles.csv'))
{
  titles <- read.csv('../rawdata/academic-titles.csv', stringsAsFactors = FALSE)
  titles <- titles[1:511, c(1, 4)]
  
  
  titles$Title[grep('PROF OF CLIN- FY', titles$Title)] <- 'PROF OF CLIN-FY'
  titles$Title[grep("RES PROF-MILLER INST -AY", titles$Title)] <- "RES PROF-MILLER INST-AY" 
  titles$Title[grep("PROF EMERITUS \\(WOS\\)", titles$Title)] <- "PROF EMERITUS(WOS)" 
  titles$Title[grep("LECT SOE-EMERITUS \\(WOS\\)", titles$Title)] <- "LECT SOE-EMERITUS(WOS)"
  
  titles$Title[grep("ASSOC IN            - AY-1/9-GSHIP" , 
                    titles$Title)] <- "ASSOC IN __ -AY-1/9-GSHIP"
  
  titles$Title[grep("ASSOC IN            -AY-1/9-NON-GSHIP", 
                    titles$Title)] <- "ASSOC IN__-AY- 1/9 -NON-GSHIP"
  
  titles$Title[grep("ASSOC IN            -AY-GSHIP" , 
                    titles$Title)] <- "ASSOC IN ____-AY-GSHIP" 
  
  titles$Title[grep("READER - GSHIP", 
                    titles$Title)] <- "READER-GSHIP" 
  
  titles$Title[grep("REMD TUT I-NON-GSHIP/NON REP", 
                    titles$Title)] <- "REMD TUT I-NON GSHIP/NON REP"
  
  titles$Title[grep("REMD TUT I-NON-GSHIP", 
                    titles$Title)] <- "REMD TUT I-NON GSHIP"
  
  titles$Title[grep("READER - NON GSHIP", 
                    titles$Title)] <- "READER-NON GSHIP"
  
  titles$Title[grep("TUT--NON STDNT/NON REP",
                    titles$Title)] <- "TUT-NON STDNT/NON REP"
  
  titles$Title[grep("READER - NON STDNT", 
                    titles$Title)] <- "READER-NON STDNT"
  
  # write interim. Some text problems from the pdf->txt process still have to be fixed by hand.
  write.csv(titles, '../data/academic-titles.csv')
}

# everything is wrapped in ifs so the files can be sourced without wasting time

###########################################################
#GENERATING CSV'S FOR EACH YEAR
###########################################################

if (!exists('titles'))
{
  titles <- read.csv('../data/academic-titles.csv', strip.white = T, stringsAsFactors = F)
}

uctca_colnames <- c("Name", "Title", "Base", "Overtime", "Other", "Benefits", "Subtotal", "Total", "Year", "Notes", "Agency")

# restricting total pay to > $1000, this removes a lot of unnamed/useless rows.
if (!file.exists('../data/uc2011.csv') || !exists('uc2011'))
{
  uc2011 <- read_csv("../rawdata/university-of-california-2011.csv", 
                     col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)
  # this is not too useful because the 2011 titles are different from the 2012:2014 titles.
  uc2011 <- left_join(uc2011, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))
  
  write.csv(uc2011, '../data/uc2011.csv')
}

if (!file.exists('../data/uc2012.csv') || !exists('uc2012'))
{
  uc2012 <- read_csv("../rawdata/university-of-california-2012.csv", 
                     col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)
  uc2012 <- left_join(uc2012, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))
  
  write.csv(uc2012, '../data/uc2012.csv')
}

if (!file.exists('../data/uc2013.csv') || !exists('uc2013'))
{
  uc2013 <- read_csv("../rawdata/university-of-california-2013.csv", 
                     col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)
  # 2013 and 2014 returned problems with read.csv(); so we terminate them with prejudice
  attributes(uc2013)$problems <- NULL
  uc2013 <- left_join(uc2013, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))
  
  write.csv(uc2013, '../data/uc2013.csv')
}

if (!file.exists('../data/uc2014.csv') || !exists('uc2014'))
{
  # for some reason this dataset has an empty column, so we delete it. Sometimes however, it does not. 
  # Because the file is so large, we are having trouble figuring out where this originated.
  uc2014 <- read_csv("../rawdata/university-of-california-2014.csv", 
                     col_names = uctca_colnames, skip = 1) %>% filter(Total > 1000)
  attributes(uc2014)$problems <- NULL
  uc2014 <- left_join(uc2014, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))
  
  write.csv(uc2014, '../data/uc2014.csv')
}








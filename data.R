titles <- read.csv('../data/academic-titles.csv', strip.white = T, stringsAsFactors = F)
#titles <- titles[-497, c('Title', 'CTO.Name')]

uc2011 <- left_join(uc2011, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))
uc2012 <- left_join(uc2012, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))
uc2013 <- left_join(uc2013, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))
uc2014 <- left_join(uc2014, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))

############################################
#FINDING AND SOLVING FOR PROBLEMS IN TITLES
############################################
#excluding recalled and visiting professors from dataset
#problems found by grep-ing 'PROF', 'GSHIP', etc. in non_acad
#problems usually from spaces and dashes

#file 'academic-titles.csv' generated from pdf 'acad-title-codes.pdf'
#csv file generated manually via Excel generating function of Adobe Acrobat
#from Excel, this was saved as a .csv file
#manually done because no available dataset not in pdf format; only way to get data in .csv format
titles = read.csv('../data/acad-title-codes.csv', stringsAsFactors = FALSE)
titles = titles[ , c(1, 4)]
titles = titles[1:496, ]


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

write.csv(titles, '../data/academic-titles.csv')

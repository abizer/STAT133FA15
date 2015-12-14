titles <- read.csv('academic-titles.csv', strip.white = T, stringsAsFactors = F)
#titles <- titles[-497, c('Title', 'CTO.Name')]

uc2011 <- left_join(uc2011, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))
uc2012 <- left_join(uc2012, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))
uc2013 <- left_join(uc2013, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))
uc2014 <- left_join(uc2014, titles, by = 'Title') %>% mutate(Academic = !is.na(Category))
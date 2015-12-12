#creating subdirectories
dir.create('code')
dir.create('rawdata')
dir.create('data')
dir.create('resources')
dir.create('report')
dir.create('images')

#downloading raw data
download.file('http://transparentcalifornia.com/export/university-of-california-2011.csv',
              'rawdata/university-of-california-2011.csv')
download.file('http://transparentcalifornia.com/export/university-of-california-2012.csv',
              'rawdata/university-of-california-2012.csv')
download.file('http://transparentcalifornia.com/export/university-of-california-2013.csv',
              'rawdata/university-of-california-2013.csv')
download.file('http://transparentcalifornia.com/export/university-of-california-2014.csv',
              'rawdata/university-of-california-2014.csv')

#downloading resource files
download.file('http://www.ucop.edu/academic-personnel-programs/_files/acad-title-codes/academic-ttles-sorted-title-name.pdf',
              'resources/acad-title-codes.pdf')
download.file('http://finreports.universityofcalifornia.edu/index.php?file=13-14/pdf/fullreport-1314.pdf',
              'resources/enrollment-data.pdf')

#creating data dictionary in rawdata subdirectory
uc2014 = read.csv('rawdata/university-of-california-2014.csv')
var.names = colnames(uc2014)
var.def = c('Names of each individual',
            'The title of the individual. Note that there are many unique title names.
            This will need to be cleaned.',
            'Base Pay',
            'Overtime Pay',
            'Other Pay',
            'Benefits',
            'Total not including benefits',
            'Total including benefits',
            'Year',
            'Notes; details if there is individual holds multiple positions',
            'Agency; all University of California employees')
var.storage = c()
for (i in colnames(uc2014)){
  sub.str = paste0('uc2014$', as.character(i), collapse = '')
  var.storage = c(var.storage, class(eval(parse(text = sub.str))))
}
var.units = c(NA, NA, 'USD', 'USD', 'USD', 'USD', 'USD', 'USD', 'Date', 'NA', 'NA')

meta = data.frame(Variable = var.names, Definition = var.def,
                  Storage = var.storage, Units = var.units, row.names = NULL)

write.csv(meta, 'rawdata/meta_data.csv')
                  
                  
                  
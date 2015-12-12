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
              'resources/enrollment-data')


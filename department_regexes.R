########################################
#
#
# DEPARTMENT REGEXES
#
#
########################################

library(hash)

ml <- hash()
ml[['Athletics']] <- '^ATH|COACH|TRAINER'
ml[['Professor']] <- 'PROF[^L]'
ml[['Administrator']] <- 'SUPV|MGR|ADMIN(ISTRATOR)?| ADM '
ml[['Academic']] <- 'ACAD(EMIC)?'
ml[['Maintenance']] <- 'MAINT|TECH|MECH|PAINTER|PLUMBER|INSTALLER|ELECTRN|SHEETMETAL|WINDOW|CUSTODIAN'
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

ml[['Lecturer']] <- 'LECT[ -]'
ml[['Adjunct Professor']] <- 'ADJ PROF'
ml[['Assistant Professor']] <- 'ASST PROF'
ml[['Associate Professor']] <- 'ASSOC PROF'
ml[['Assistant Adjunct Professor']] <- 'ASST ADJ PROF'
ml[['Associate Adjunct Professor']] <- 'ASSOC ADJ PROF'
ml[['Professor']] <- 'PROF[^L]'
ml[['Post Doc']] <- 'POSTDOC'

ml[['Vice President']] <- 'S?VP '
ml[['Vice Chancellor']] <- '^VC |VICE CHAN'
ml[['Chancellor']] <- '^CHAN'
ml[['Researcher']] <- 'RSCH'
ml[['Veterinarian']] <- 'VETERINARIAN'
ml[['Professional']] <- 'PROFL'
ml[['Teacher']] <- 'TEACHER|EDUCATOR'
ml[['Dean']] <- 'DEAN'
ml[['Officer']] <- 'OFCR'
ml[['Marine']] <- 'SEAMAN|(SEA CAPTAIN)|^MARINE|BOATSWAIN' # you can be ordinary or able as far as seamen are concerned
ml[['Analyst']] <- 'ANL' # space intentional
ml[['Therapist']] <- ' THER '
ml[['Consultant']] <- 'CNSLT|CONSULT'
ml[['Counselor']] <- 'CNSLR'
ml[['Medical Misc.']] <- '^MED |^HOSP |^PATIENT |^OPERATING |HEALTH|TRANSPLANT'
ml[['Library Misc.']] <- 'LIBRAR.+'
ml[['Operator']] <- 'OPR'
ml[['Technologist']] <- 'TCHL'
ml[['Physician | PA']] <- 'PHYS(CN)? ?(?!PLT)' # space optional 
ml[['Medical Resident']] <- '^RES-'
ml[['Programmer']] <- 'PROGR [0-9]'
ml[['Investments']] <- ' INV |REAL ESTATE'
ml[['Media/Marketing']] <- 'MEDIA|MARKETING'
ml[['Legal']] <- 'LEGAL|ATTORNEY|COUNSEL'
ml[['Lab Worker']] <- '^LAB'
ml[['Management']] <- 'MGT'
ml[['Clinial']] <- 'CLIN'
ml[['Police']] <- 'POLICE'
ml[['Random']] <- paste0('MAIL|MASSAGE|MASON|FARM |MASON|LINEN|OILER|',
                         'WIPER|USHER|INSULATION|HIGH VOLT|FUNDRAISER|',
                         'BLANK |BUYER|ARTIST|ART MODEL|AGRON|',
                         'WRITER|TRAVEL|TREE |WIPER|USER EXPER|ZONE|',
                         'EDITOR|WORD PROCESSING|SURVEY|STEAMFITTER|',
                         'GLASSBLOWER|STAGE HELPER|STOREKEEPER|THEATER|',
                         'STUDIO|ROOFER|MISCELLANEOUS|MACHINIST|',
                         'LOCKSMITH|AUTO ATTENDANT|CABINET MAKER|',
                         'GLAZIER|TRUCK|DRIVER|LIFEGUARD|MEETING|SECURITY|PHOTOGRAPHER|PARKING|',
                         'DELIVERY')

ml[['Medical Professional']] <- paste0('AUDIOLOGIST|PSYCHO(LOG(Y|IST)|METRIST)|DENT(AL|IST)|',
                                       'DIETITIAN|PREVENTIONIST|OPTICIAN|',
                                       'OPTOMETRIST|ORTHOPTIST|PERFUSIONIST|',
                                       'PHARMACIST|PHLEBOTOMIST|PATHOLOGIST|',
                                       'SPECTROSCOPIST|PROSTHETIST|DOSIMETRIST')
ml[['Public Service']] <- '^PUBL |SOCIAL '
ml[['Executive']] <- '^EXEC '
ml[['Statistician']] <- 'STATISTICIAN'
ml[['Translator']] <- 'TRANSLATOR'
ml[['Museum']] <- 'MUSEUM'
ml[['Ombudsperson']] <- 'OMBUDSPERSON'
ml[['Grounds']] <- 'LANDSCAPE|GROUNDS|IRRIGATION'
ml[['Med Execs']] <- 'MED CTR'
ml[['Inspector']] <- 'INSP'
ml[['Director']] <- 'DIR'
ml[['Fire']] <- 'FIRE'
ml[['Food']] <- 'FOOD|CHEF|COOK|BAKER'
ml[['Children']] <- 'CHILD'
ml[['Provost']] <- 'PROVOST'


ml[['Arts']] <- 'MUSICIAN|ILLUSTRATOR'
ml[['Employee Rel']] <- 'EMPLOY(EE|MENT)'
ml[['Radiology']] <- 'RADLG'

ml[['Management Uncategorized']] <- paste0('ASC OF PRES OR CHAN|ASSO V CHAN|ASC TO CHAN|AST TREASURER|ASC VP|',
                                           'PRESIDENT OF THE UNIV|SECR OF THE REGENTS|SENATE EMERITUS|',
                                           'TREASURER OF THE REGENTS')

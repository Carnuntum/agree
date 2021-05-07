
docMenu <- menuItem('Documentation', tabName = 'docum',
                    icon = icon('info-circle')
                    )

gettingStarted <- menuItem('Getting Started', tabName = 'decTree')

allMethods <- menuItem(text = 'Methods', 
                       tabName = 'methods',
                       icon = icon('chart-bar'),
                       
                       #SCOTTS PI
                       menuItem(text = "Scott's Pi", tabName = 'kappa_pi'),
                       
                       #COHENS KAPPA
                       menuItem(text = "Cohen's Kappa", tabName = 'kappa_cohen'),
                       
                       #RAND INDEX
                       menuItem(text = 'Adjusted Rand Index', tabName = 'other_randIndex'),
                       
                       #CONGERS KAPPA
                       menuItem(text = "Conger's Kappa", tabName = 'kappa_conger'),
                       
                       #FLEISS KAPPA
                       menuItem(text = "Fleiss' Kappa", tabName = 'kappa_fleiss'),
                       
                       #RWG
                       menuItem(text = "Reliability Within Group Index (RWG)", tabName = 'other_rwg'),
                       
                       #AWG
                       menuItem(text = "Agreement Within Group Index (AWG)", tabName = 'other_awg'),
                       
                       #BANGDIWALA B
                       menuItem(text = "Bangdiwala's B", tabName = 'other_bangdiwala'),
                       
                       #AICKIN ALPHA
                       menuItem(text = "Aickin's Alpha", tabName = 'kappa_aickin'),
                       
                       #BRENNAN PREDIGER
                       menuItem(text = 'Brennan & Prediger\'s Kappa', tabName = 'kappa_brennan'), 
                       
                       #BYRT KAPPA
                       menuItem(text = "Byrt's Kappa", tabName = 'kappa_byrt'),
                       
                       #IOTA
                       menuItem(text = "Iota", tabName = 'other_iota'),
                       
                       #GWET
                       menuItem(text = "Gwets AC", tabName = 'kappa_gwet'),
                       
                       #ENTROPY
                       menuItem(text = "Entropy Index", tabName = 'other_entropy'),
                       
                       #VON EYE KAPPA
                       menuItem(text = "Von Eye's Kappa", tabName = 'kappa_eye'),
                       
                       #AD INDEX
                       menuItem(text = "Agreement Coefficient d", tabName = 'other_ad'),
                       
                       #A KAPPA
                       menuItem(text = "A Kappa", tabName = 'kappa_akappa'),
                       
                       #FREE KAPPA CARPENTIER
                       menuItem(text = "Carpentier's Kappa", tabName = 'kappa_free'),
                       
                       #INFORMATION AGREEMENT
                       menuItem(text = "Information Agreement", tabName = 'other_infoAgree'),
                       
                      #replace
                      menuItem(text = 'Intraclass Correlation',
                               tabName = 'icc'),
                      
                      #replace
                      menuItem(text = "Sklars Omega",
                               tabName = 'omega'),
                      
                      #replace
                      menuItem(text = "Krippendorfs's Alpha",
                               tabName = 'krippendorf'),
                      
                      #replace
                      menuItem(text = 'Concordance Correlation Coeff.',
                               tabName = 'other_occc'))
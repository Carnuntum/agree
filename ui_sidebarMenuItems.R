
docMenu <- menuItem('Documentation', tabName = 'docum',
                    icon = icon('info-circle'),
                    selected = T
                    )

allMethods <- menuItem(text = 'Methods', 
                       tabName = 'methods',
                       icon = icon('chart-bar'),
                       
                       menuItem(text = 'Nominal Scale Data',
                                tabName = 'nominal',
                                startExpanded = T,
                                
                         menuItem(text = 'Chi-square', tabName = 'chi'),
                         
                         
                         menuItem(text = 'Percent Agreement', 
                                  tabName = 'overallPA',
                                  startExpanded = T,
                                  
                                  menuItem(text = 'Total PA',
                                           tabName = 'percAgree'),
                                  menuItem(text = 'Category Specific PA',
                                           tabName = 'percAgrPN')),
                         
                         
                         menuItem(text = "Cohen's Kappa",
                                  tabName = 'cohenk'),
                         
                         menuItem(text = "Odds Ratio", tabName = 'odds',
                                  selected = F)),
                       
                       
                       menuItem(text = 'Ordinal Scale Data',
                                tabName = 'ordinal',
                                startExpanded = T,
                          menuItem(text = "Rank Measures",
                                   tabName = 'ordRank'),
                          menuItem(text = 'Polychoric Correlation',
                                   tabName = 'polyc',
                                   selected = F)),
                       
                       menuItem(text = 'Interval Scale Data',
                                tabName = 'interval',
                                startExpanded = T,
                          menuItem(text = 'Intraclass Correlation',
                                   tabName = 'icc'),
                          menuItem(text = "Sklars Omega",
                                   tabName = 'omega',
                                   selected = F),
                          menuItem(text = "Krippendorfs's Alpha",
                                   tabName = 'krippendorf',
                                   selected = F),
                          menuItem(text = 'Concordance Correlation Coeff.',
                                   tabName = 'CCC',
                                   selected = F)))
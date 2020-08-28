
docMenu <- menuItem('Documentation', tabName = 'docum', 
                          icon = icon('info-circle'))

allMethods <- menuItem(text = 'Methods', 
                       tabName = 'methods',
                       icon = icon('chart-bar'),
                       
                       menuItem(text = 'Nominal Scale Data',
                                tabName = 'nominal',
                                
                         menuItem(text = 'Chi-square', tabName = 'chi'),
                         
                         
                         menuItem(text = 'Percent Agreement', 
                                  tabName = 'overallPA',
                                  menuItem(text = 'Total PA',
                                           tabName = 'percAgree'),
                                  menuItem(text = 'Category Specific PA',
                                           tabName = 'percAgrPN')),
                         
                         menuItem(text = "Cohen's Kappa",
                                  tabName = 'cohenk')),
                       
                       menuItem(text = 'Ordinal Scale Data',
                                tabName = 'ordinal',
                                startExpanded = T,
                          menuItem(text = "Rank Measures",
                                   tabName = 'ordRank',
                                   selected = T)))
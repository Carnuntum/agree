
docMenu <- menuItem('Documentation', tabName = 'docum', 
                          icon = icon('angle-rigt'))

allMethods <- menuItem(text = 'Methods', 
                       tabName = 'methods',
                       
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
                          menuItem(text = "Spearman's Rho",
                                   tabName = 'spear',
                                   selected = T)))
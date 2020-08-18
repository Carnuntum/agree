

header <- dashboardHeader(title = 'Agreement Measures', 
                          tags$li(class = "dropdown", style = 'padding: 0;',
                                  dropdownButton(inputId = 'settings',
                                               label = 'settings',
                                               width = '160px',
                                               circle = F,
                                               actionButton(inputId = 'color', 
                                                            label = 'darkmode',
                                                            width = '140px',
                                                            style = '
                                                            height: 30px;
                                                            font-size: 15px;'),
                                               
                                               actionButton(inputId = 'changCol', 
                                                            label = 'change hover',
                                                            width = '140px',
                                                            style = '
                                                            margin-top: 10px;
                                                            height: 30px;
                                                            font-size: 15px;'),
                                               
                                               #NAVIGATION BACK TO HOME TAB
                                               actionButton(inputId = 'home',
                                                            label = 'home',
                                                            width = '140px',
                                                            style = '
                                                            margin-top: 10px;
                                                            height: 30px;
                                                            font-size: 15px;'),
                                               right = T,
                                               status = 'settBtn'
                                  
                                  )))


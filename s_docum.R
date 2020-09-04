source('text.R')

documentation <- tabItem(tabName = 'docum',
                         fluidRow(
                           column(
                             width = 10,
                             offset = 1,
                             style = 'text-align: center;',
                             titlePanel('Agreement Measures Calculator'),
                             fluidRow(class = 'homeDocumBtns',
                               actionButton(inputId = 'imp_text',
                                            label = 'read me first',
                                            style = 'font-size: 35px;'),
                               actionButton(inputId = 'imp_text2',
                                            label = 'read me next',
                                            style = 'font-size: 35px;'),
                               actionButton(inputId = 'imp_text3',
                                            label = 'now read me!',
                                            style = 'font-size: 35px;')
                             )
                           )
                         ),
                         
                         hidden(
                           div(id = 'test',
                               fluidRow(class = 'documRow',
                                        column(width = 6,
                                               offset = 3,
                                               box(title = lorem,
                                                   width = NULL)
                                               )
                                        )
                               )
                           )
                         )
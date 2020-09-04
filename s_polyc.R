source('src_functions.R')
source('style.R')

polychor <- tabItem(tabName = 'polyc',
                       fluidRow(
                         column(
                           width = 10,
                           offset = 1,
                           style = 'padding-left: 0px; padding-right: -5px;',
                           box(
                             width = NULL,
                             style = 'text-align:center; padding: 30px;',
                             h3("Polychoric Correlation for Ordinal Measures")
                           )
                         )
                       ),
                       fluidRow(
                         column(
                           width = 5,
                           offset = 1,
                           fluidRow(
                             box(
                               width = NULL,
                               height = '105px',
                               p('On the right side you can upload your 2 by 2 
                                 or 2 by n data via a .csv file and type the 
                                 number of categories for which ratings were 
                                 applied'),
                               style = 'text-align: center;'
                             )
                           ),
                           fluidRow(
                             box(
                               width = NULL,
                               height = '105px',
                               p('The data has to have the following structure:'),
                               style = 'text-align: center;'
                             )
                           )
                         ),
                         column(
                           width = 5,
                           box(width = NULL,
                               tags$script(js_upload_complete), #custom js to change upload complete text
                               fileInput(inputId = 'polycInput',
                                         label = 'Browse for .csv files',
                                         accept = ".csv"),
                               style = 'text-align: center;',
                               actionButton(inputId = 'polycRun',
                                            label = "Run")
                           )
                         )
                       ),
                       fluidRow(class = 'tabStyle',
                         column(
                           width = 5, 
                           offset = 1,
                           style = 'padding: 0px;',
                           tabsetPanel(
                             tabPanel(title = 'Example 1',
                               box(
                                 title = '', 
                                 width = NULL,
                                 tableOutput('expPolyc1'),
                                 actionButton(
                                   inputId = 'polycTest1',
                                   label = 'test run',
                                   style = 'float:right;'),
                                 style = 'text-align: center;'
                               )
                              ),
                             tabPanel(title = 'Example 2',
                               box(
                                 title = '', 
                                 width = NULL,
                                 tableOutput('expPolyc2'),
                                 actionButton(
                                   inputId = 'polycTest2',
                                   label = 'test run',
                                   style = 'float:right;'),
                                 style = 'text-align: center;'
                               )
                             )
                             )
                           ),
                         column(
                           width = 5,
                           fluidRow(class = 'style_valuebox_POLYC_cyan',
                                    column(
                                      width = 12,
                                      style = 'text-align: center;',
                                      valueBoxOutput(
                                        outputId = 'polyc1',
                                        width = NULL)))
                           
                         )
                       )
)

#-------------------------------------------------------------------------------
#'*------------------------- DEFAULT OUTPUT -----------------------------------*
#-------------------------------------------------------------------------------

polycDefOut <- function(inp, out) {
  output$polyc1 <- renderValueBox({
    
    valueBox(value = h4("Output",
                        style = 'text-align: center;
                                padding-top: 10px;
                                font-size: 25px;'), 
             '')
    
  })
}

#-------------------------------------------------------------------------------
#'*------------------------- CALCULATED OUTPUT --------------------------------*
#-------------------------------------------------------------------------------

polycOut <- function(input, output, data) {
  
  test <- polyc(data)
  rho <- round(test$values$rho, 3)
  rho[upper.tri(rho)] <- ''
  warn <<- test$warn
  
  
  output$polyc1 <- renderValueBox({
    
    valueBox(value = h4("Polychoric Correlation Rho",
                        style = 'text-align: center;
                                padding-top: 10px;
                                font-size: 25px;'),
             subtitle = p(HTML(
               paste0(
                 if(!is.matrix(rho)) {
                   paste0('Rho: ', rho,
                          br(),
                          br(),
                          'Tau Thresholds: ',
                          br(),
                          kable(data.frame('row_thres' = test$values$tau.row,
                                           'col_thres' =test$values$tau.col),
                                'html') %>% 
                            kable_styling('basic',
                                          font_size = 18)
                          )
                   }
                 else if (is.matrix(rho)) {
                   paste0(
                     kable(rho, format = 'html') %>%
                       kable_styling(bootstrap_options = 'basic',
                                     font_size = 18),
                     br(),
                     p('Tau Thresholds: ', style = 'font-size: 18px;'),
                     br(),
                     kable(round(test$values$tau, 3), 'html') %>% 
                       kable_styling(bootstrap_options = 'basic',
                                     font_size = 18),
                     if(!is.null(test$warn)) {
                       circleButton(inputId = 'polycWarn',
                                    icon = icon("exclamation"),
                                    size = 's')
                     }
                     )
                   }
                 )
               ),
             style = 'text-align: left;
                      font-size: 18px;')
            )
    
  })
  
}


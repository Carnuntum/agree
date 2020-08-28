source('src_functions.R')

paData <- expression(reactive({ #user input data
  read.csv(input$paInput$datapath)
}))

pnData <- expression(reactive({
  read.csv(input$pnInput$datapath)
}))

#-------------------------------------------------------------------------------
#'*------------------------ PERCENT AGREEMENT TABITEM -------------------------*
#-------------------------------------------------------------------------------


percentAgree <- tabItem(
  tabName = 'percAgree',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    width = NULL,
                    style = 'text-align:center; padding: 30px;',
                    h3('Percent Agreement for 2 by \
                       2 or 2 by n contingency tables')
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
          p(
            'On the right side you can upload your 2 by 2 or 2 by n data \
          via a .csv file and type the number of categories for which ratings \
          were applied'
          ),
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
    )
    ,
    column(
      width = 5,
      box(
        width = NULL,
        height = '230px',
        fileInput(inputId = 'paInput',
                  label = 'Browse for .csv files'),
        actionButton(
          inputId = 'paRun',
          label = 'run',
          style = 'margin-left: 50%;'
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 5,
      offset = 1,
      style = 'padding: 0px;',
      box(
        title = 'Data Example',
        width = NULL,
        tableOutput('expPA'),
        actionButton(
          inputId = 'paTest',
          label = 'test run',
          style = 'float: right;'
        )
      )
    ),
    
    column(width = 5,
           fluidRow(class = 'style_valuebox_PA_cyan',
                    column(
                      width = 12,
                      valueBoxOutput(outputId = 'paTotal',
                                     width = NULL)
                    )
           ),
           fluidRow(class = 'style_valuebox_PA_cyan',
                    column(
                      width = 12,
                      valueBoxOutput(outputId = 'paExpected',
                                     width = NULL)
                    )
           ),
           fluidRow(class = 'style_valuebox_PA_cyan',
                    column(
                      width = 12,
                      valueBoxOutput(outputId = 'paUncategorized',
                                     width = NULL)
                    )
           )
    )
  )
)

percAgrPN <- tabItem(tabName = 'percAgrPN',
                     fluidRow(
                       column(
                         width = 10,
                         offset = 1,
                         style = 'padding-left: 0px; padding-right: -5px;',
                         box(
                           width = NULL,
                           style = 'text-align:center; padding: 30px;',
                           h3('Percent Agreement for 2 by \
                       2 or 2 by n contingency tables')
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
          via a .csv file'),
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
                       )
                       ,
                       column(width = 5,
                              box(
                                width = NULL,
                                height = '230px',
                                fileInput(inputId = 'pnInput',
                                          label = 'Browse for .csv files'),
                                actionButton(
                                  inputId = 'pnRun',
                                  label = 'run',
                                  style = 'margin-left: 50%;'
                                )
                              ))
                     ),
                     fluidRow(class = 'tabStyle',
                       column(
                         width = 5,
                         offset = 1,
                         style = 'padding: 0px;',
                         tabsetPanel(
                           tabPanel(title = 'tab1',
                           box(
                             title = 'Data Example 1',
                             width = NULL,
                             tableOutput('expPN'),
                             actionButton(
                               inputId = 'pnTest',
                               label = 'test run',
                               style = 'float: right;'
                             )
                           )),
                           tabPanel(title = 'tab2',
                                    box(
                                      title = 'Data Example 2',
                                      width = NULL
                                    ))
                         )
                       ),
                       
                       column(
                         width = 5,
                         fluidRow(class = 'style_valuebox_PA_cyan',
                                  column(
                                    width = 12,
                                    valueBoxOutput(outputId = 'pn',
                                                   width = NULL)
                                  )),
                         fluidRow(class = 'style_valuebox_PA_cyan',
                                  column(
                                    width = 12,
                                    valueBoxOutput(outputId = 'pnCond',
                                                   width = NULL)
                                  )),
                         fluidRow(class = 'style_valuebox_PA_cyan',
                                  column(
                                    width = 12,
                                    valueBoxOutput(outputId = 'kappa_pa',
                                                   width = NULL)
                                  )),
                         fluidRow(class = 'style_valuebox_PA_cyan',
                                  column(
                                    width = 12,
                                    valueBoxOutput(outputId = 'x2_pa',
                                                   width = NULL)
                                  )),
                         fluidRow(class = 'style_valuebox_PA_cyan',
                                  column(
                                    width = 12,
                                    valueBoxOutput(outputId = 'mcnemar_pa',
                                                   width = NULL)
                                  ))
                       )
                     ))

#-------------------------------------------------------------------------------
#'*------------------- PERCENT AGREEMENT SERVER LOGIC -------------------------*
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#'*--------------- PERCENT AGREEMENT EXAMPLE TABLE DATA -----------------------*
#-------------------------------------------------------------------------------
# paExampleTable <- function(input, output) {
#   output$expPA <- function() {
#     kable(percAgrTableExp, format = 'html') %>%
#       kable_styling(bootstrap_options = 'basic')
#   }
# }
# 
# paExampleTablePN <- function(input, output) {
#   output$expPN <- function() {
#     kable(percAgrPosNeg, format = 'html') %>%
#       kable_styling(bootstrap_options = 'basic')
#   }
# }

#-------------------------------------------------------------------------------
#'*---------------- DEFAULT INFORMATION FOR PA OUTPUT BOXES -------------------*
#-------------------------------------------------------------------------------
paOutDefault <- function(inp, out) {
  output$paTotal <- renderValueBox({
    
      valueBox(value = h4('Total Percent Agreement',
                          style = 'text-align: center;
                                  padding: 15px;'), '')
    
  })
  output$paExpected <- renderValueBox({
    
      valueBox(value = h4('Expected Percent Agreement',
                          style = 'text-align: center;
                                  padding: 15px;'), '')
    
  })
  output$paUncategorized <- renderValueBox({
    
    valueBox(value = h4('Percent Agreement With Uncategorizations',
                        style = 'text-align: center;
                                  padding: 15px;'), '')
  })
  
  output$pn <- renderValueBox({
    
    valueBox(value = h4('Positive/Negative Percent Agreement',
                        style = 'text-align: center;
                                  padding: 15px;'), '')
  })
  
  output$pnCond <- renderValueBox({
    
    valueBox(value = h4('Conditional Positive/Negative PA',
                        style = 'text-align: center;
                                  padding: 15px;'), '')
  })
  
  output$kappa_pa <- renderValueBox({
    
    valueBox(value = h4('Category Specific Kappa Coefficient',
                        style = 'text-align: center;
                                  padding: 15px;'), '')
  })
  
  output$x2_pa <- renderValueBox({
    
    valueBox(value = h4('Chi Square Test for Significance',
                        style = 'text-align: center;
                                  padding: 15px;'), '')
  })
  
  output$mcnemar_pa <- renderValueBox({
    
    valueBox(value = h4('McNemar Test For Difference In Marginal Distribution',
                        style = 'text-align: center;
                                  padding: 15px;'), '')
  })
}

#-------------------------------------------------------------------------------
#'*--------------- CALCULATED OUTPUT FOR PERCENT AGREEMENT --------------------*
#-------------------------------------------------------------------------------
#chi output for corrected test
paOut <- function(input, output, data) {
  
  test <- tryCatch({
    agreeCorFor2byN(data)
  }, error = function(e) {
    print(e)
  })
  
  output$paTotal <- renderValueBox({
    
    valueBox(
      value = h4('Total Percent Agreement',
                         style = 'text-align: center;
                                  padding-top: 10px;')
      ,
      subtitle = 
        p(
          HTML(
            paste0(
              round(as.numeric(test$total), 3),
              br()
        )
      ),
      style = 'text-align: center;
              font-size: 25px;')
    )
  })
  
    
  output$paExpected <- renderValueBox({
    valueBox(
      value = h4('Expected Percent Agreement',
                 style = 'text-align: center;
                                  padding-top: 10px;')
      ,
      subtitle =
        h5(HTML(paste0(round(
          as.numeric(test$expected), 3
        ), '<br/>')
      ),
      style = 'text-align: center;
              font-size: 25px;'))
  })
  
  output$paUncategorized <- renderValueBox({
    valueBox(
      value = h4('Percent Agreement With Uncategorizations',
                 style = 'text-align: center;
                          padding-top: 10px;')
      ,
      subtitle = 
        if(test$uncat == F) {h5('No Missing Categorizations',
                                style = 'text-align: center;')}
        else {
          h5(HTML(
            paste0(
              round(as.numeric(test$uncategorized), 3),'<br/>'
            )
          ),
          style = 'text-align: center;
                  font-size: 25px;')
        }
    )
  })
}


pnOut <- function(input, output, data) {
  
  test <- tryCatch({
    PercentAgreeFor2by2(data)
  }, error = function(e) {
    print(e)
  })
  
  homoPA <- mcnemar.test(data[,1], data[,2])
  
  output$pn <- renderValueBox({
    valueBox(
      value = h4('Positive/Negative PA',
                 style = 'text-align: center;
                                  padding-top: 10px;')
      ,
      subtitle =
        h5(HTML(paste0('+ ', round(test$paPos, 3), '<br/>',
                       '- ', round(test$paNeg, 3))
        ),
        style = 'text-align: center;
              font-size: 25px;'))
  })
  
  output$pnCond <- renderValueBox({
    valueBox(
      value = h4('Conditional Positive/Negative PA',
                 style = 'text-align: center;
                                  padding-top: 10px;')
      ,
      subtitle =
        h5(HTML(paste0('+ ', round(test$paPosSF, 3), '<br/>',
                       '- ', round(test$paNegSF, 3))
        ),
        style = 'text-align: center;
              font-size: 25px;'))
  })
  
  output$kappa_pa <- renderValueBox({
    valueBox(
      value = h4('Positive/Negative Kappa Coefficient',
                 style = 'text-align: center;
                                  padding-top: 10px;')
      ,
      subtitle =
        h5(HTML(paste0('+ ', round(as.numeric(test$kPos), 3), '<br/>',
                       '- ', round(as.numeric(test$kNeg), 3))
        ),
        style = 'text-align: center;
              font-size: 25px;'))
  })
  
  
  output$x2_pa <- renderValueBox({
    valueBox(
      value = h4('Chi Square Test for Significance',
                 style = 'text-align: center;
                                  padding-top: 5px;')
      ,
      subtitle =
        h5(HTML(paste0(lapply(text_percAgrPN, eval, environment()))
        ),
        style = 'text-align: center;
              font-size: 18px;'))
  })
  
  output$mcnemar_pa <- renderValueBox({
      valueBox(
        width = 10,
        value = h4(paste0('McNemar Test for Homogeneity'),
                   style = 'text-align: center;
                            padding-top: 5px;')
        ,
        subtitle =
          h5(HTML(paste0('X', tags$sup('2'), ' = ', round(homoPA$statistic, 3),
                         br(),
                         'p = ', round(homoPA$p.value, 3), 
                         br(),
                         'df = ', round(homoPA$parameter, 3))
          ),
          style = 'text-align: center;
                   font-size: 18px;'))
      
  })
  
}

#-------------------------------------------------------------------------------
#'*------------------------- ERROR MESSAGES FOR USER --------------------------*
#-------------------------------------------------------------------------------
btnPressWithoutData <- function() {
  showModal(
    modalDialog(
      title = 'Ups.. something went wrong!',
      'You pressed run without supplying any data!',
      easyClose = T,
      style = 'text-align: center;'
    )
  )
}

pa_wrongCatNumber <- function() {
  showModal(
    modalDialog(
      title = 'Something went wrong!',
      'The categories integer you entered seems to be wrong!',
      easyClose = T,
      style = 'text-align: center;'
    )
  )
}
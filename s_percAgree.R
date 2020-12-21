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
                    h3('Total Percent Agreement')
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
          p(file_upload_text),
          style = 'text-align: center;'
        )
      ),
      fluidRow(
        box(
          width = NULL,
          height = '105px',
          p(file_struct_text),
          look_down,
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
    class = 'tabStyle',
    column(
      width = 5,
      offset = 1,
      style = 'padding: 0px;',
      uiOutput('ui_pa')
    ),
    
    column(width = 5,
           fluidRow(class = 'style_valuebox_PA_cyan',
                    column(
                      width = 12,
                      valueBoxOutput(outputId = 'pa',
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
                           h3('Category Specific Percent Agreement')
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
                             p(file_upload_text),
                             style = 'text-align: center;'
                           )
                         ),
                         fluidRow(
                           box(
                             width = NULL,
                             height = '105px',
                             p(file_struct_text),
                             look_down,
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
                         uiOutput('ui_pn')
                       ),
                       
                       column(
                         width = 5,
                         fluidRow(class = 'style_valuebox_PA_cyan',
                                  column(
                                    width = 12,
                                    valueBoxOutput(outputId = 'pn',
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
# paOutDefault <- function(inp, out) {
#   output$paTotal <- renderValueBox({
#     
#       valueBox(value = h4('Total Percent Agreement',
#                           style = 'text-align: center;
#                                   padding: 15px;'), '')
#     
#   })
#   output$paExpected <- renderValueBox({
#     
#       valueBox(value = h4('Expected Percent Agreement',
#                           style = 'text-align: center;
#                                   padding: 15px;'), '')
#     
#   })
#   output$paUncategorized <- renderValueBox({
#     
#     valueBox(value = h4('Percent Agreement With Uncategorizations',
#                         style = 'text-align: center;
#                                   padding: 15px;'), '')
#   })
#   
#   output$pn <- renderValueBox({
#     
#     valueBox(value = h4('Positive/Negative Percent Agreement',
#                         style = 'text-align: center;
#                                   padding: 15px;'), '')
#   })
#   
#   output$pnCond <- renderValueBox({
#     
#     valueBox(value = h4('Conditional Positive/Negative PA',
#                         style = 'text-align: center;
#                                   padding: 15px;'), '')
#   })
#   
#   output$kappa_pa <- renderValueBox({
#     
#     valueBox(value = h4('Category Specific Kappa Coefficient',
#                         style = 'text-align: center;
#                                   padding: 15px;'), '')
#   })
#   
#   output$x2_pa <- renderValueBox({
#     
#     valueBox(value = h4('Chi Square Test for Significance',
#                         style = 'text-align: center;
#                                   padding: 15px;'), '')
#   })
#   
#   output$mcnemar_pa <- renderValueBox({
#     
#     valueBox(value = h4('McNemar Test For Difference In Marginal Distribution',
#                         style = 'text-align: center;
#                                   padding: 15px;'), '')
#   })
# }

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
  
  d_pa <- t(data.frame(
    'Total Percent Agreement' = test$total,
    'Expected Percent Agreement' = test$expected,
    'PA With Uncategorization' = if(!is.null(test$uncategorized)) {
      test$uncategorized
    } else {
      'no missings'  
    }
  ))
  
  l_pa <<- lapply(d_pa, as.data.frame)
  
  output$pa <- renderValueBox({
    
    valueBox(
      value = p(HTML(
        kableExtra::kable(d_pa, format = 'html') %>% 
          kableExtra::kable_styling('basic'),
        
      ),
      div(
        downloadButton(outputId = 'paFullDown',
                       label = 'Full Results'),
        style = 'text-align: center;'
      )),
      subtitle = ''
    )
  })
}


pnOut <- function(input, output, data) {
  
  test <- tryCatch({
    PercentAgreeFor2by2(data)
  }, error = function(e) {
    print(e)
  })
  
  homoPA <- tryCatch({
    mcnemar.test(data[,1], data[,2])
  }, error = function(e) {
    print(e)
  }, warning = function(w) {
    print(w)
  })
  
  d_pn <- t(data.frame(
    'pos' = test$paPos,
    'neg' = test$paNeg,
    'cond.pos' = test$paPosSF,
    'cond.neg' = test$paNegSF,
    'kappa pos' = test$kPos,
    'kappa neg' = test$kNeg,
    'chi uncorr' = test$chiUncor,
    'chi corr' = test$chiValCor,
    'p-value uncorr' = test$chipUncor,
    'p-value corr' = test$chipCor,
    'df uncorr' = test$chidfUncor,
    'df corr' = test$chidfCor,
    'chi homogeneity' = homoPA$statistic,
    'p-value' = homoPA$p.value,
    'df' = homoPA$parameter
  ))
  dimnames(d_pn)[2] <- NULL
  
  d <<- d_pn
  
  l_pn <<- lapply(d_pn, as.data.frame)
  
  output$pn <- renderValueBox({
    valueBox(
      value = p(HTML(
        kableExtra::kable(round(d_pn, 3), format = 'html') %>% 
          kableExtra::kable_styling('basic'),
        
      ),
      div(
        downloadButton(outputId = 'pnFullDown',
                       label = 'Full Results'),
        style = 'text-align: center;'
      )),
      subtitle = ''
    )
  })
  
}


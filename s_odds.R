odds <- tabItem(tabName = 'odds',
               useShinyjs(),
               fluidRow(
                 column(
                   width = 10,
                   offset = 1,
                   style = 'padding-left: 0px; padding-right: -5px;',
                   box(
                     width = NULL,
                     style = 'text-align:center; padding: 30px;',
                     h3(id = 'oddsDocum',
                        "Odds Ratio")
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
                       height = '131px',
                       p(file_upload_text),
                       style = 'text-align: center;'
                     )
                   ),
                   fluidRow(
                     box(
                       width = NULL,
                       height = '131px',
                       p('The data has to have the following structure:'),
                       style = 'text-align: center;'
                     )
                   )
                 ),
                 column(
                   width = 5,
                   box(width = NULL,
                       #custom js to change upload complete text
                       fileInput(inputId = 'oddsInput',
                                 label = 'Browse for .csv files',
                                 accept = ".csv"),
                       style = 'text-align: center;',
                       actionButton(inputId = 'oddsRun',
                                    label = 'calculate')
                   )
                 )
               ),
               fluidRow(class = 'tabStyle',
                        column(
                          width = 5, 
                          offset = 1,
                          style = 'padding: 0px;',
                          uiOutput('ui_odds')
                        ),
                        column(
                          width = 5,
                          fluidRow(class = 'style_valuebox_ODDS_cyan',
                                   column(
                                     width = 12,
                                     valueBoxOutput(
                                       outputId = 'odds1',
                                       width = NULL)
                                   )
                          )
                        )
               )
)

oddsMainOut <- function(input, output, data) {
  
  tryCatch({
    test <- oddsMain(data, alpha = 0.05)
    
    l_odds <<- as.list(lapply(test, as.data.frame))
    
  }, error = function(e) {
    print(e)
  }, warning = function(w) {
    print(w)
  })

  output$odds1 <- renderValueBox({
    tryCatch({
      
      d_odds <- t(data.frame(
        'Odds' = round(test$oddsRatio, 3), 
        'zCrit: ' =  round(test$zCrit, 3), 
        'log Odds' =  round(test$logOdds, 3), 
        'log Standard Error' =  round(test$logStdErr, 3), 
        'zLog' =  round(test$zLog, 3), 
        'zYule' =  round(test$zYule, 3), 
        "Yule's Y" =  round(test$yuleY, 3), 
        'p ln' =  round(test$pLN, 3), 
        'log lower bound' =  round(test$logLB, 3), 
        'log upper bound' =  round(test$logUB, 3), 
        'odds lower' =  round(test$oddsLB, 3), 
        'odd upper' =  round(test$oddsUB, 3), 
        'Y lower' =  round(test$yuleLB, 3), 
        'Y upper' =  round(test$yuleUB, 3), 
        'transformed odds lb' =  round(test$transformedOddsLB, 3),
        'tranformed odds ub' =  round(test$transformedOddsUB, 3)
      ))
  
      valueBox(subtitle = p(HTML(paste0(
        kableExtra::kable(d_odds, format = 'html') %>% 
          kableExtra::kable_styling('basic'),
        br(),
        div(
          downloadButton(outputId = 'oddsFullDown',
                         label = 'full result'),
          style = 'text-align: center;'),
        br()
        
        
      )), style = 'text-align: center; color: white;'),
      value = ''
      )
      
    }, error = function(e) {
      print(e)
      oddsErrOut(output)
    }, warning = function(w) {
      print(w)
      oddsErrOut(output)
    })
  })
}
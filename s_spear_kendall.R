source('src_functions.R')
source('style.R')

ordinalRank <- tabItem(tabName = 'ordRank',
                        fluidRow(
                          column(
                            width = 10,
                            offset = 1,
                            style = 'padding-left: 0px; padding-right: -5px;',
                            box(
                              width = NULL,
                              style = 'text-align:center; padding: 30px;',
                              h3("Ordinal Rank Measures for 2 Raters")
                              )
                            )
                          ),
                        fluidRow(
                          column(
                            width = 5, 
                            offset = 1,
                            style = 'padding: 0px;',
                            box(
                              width = NULL,
                              height = '200px',
                              h5('On the right side you can upload your data 
                                 via a .csv file.'),
                              h5('The file has to have the following 
                                 structure:'),
                              style = 'text-align: center;')
                            ),
                          column(
                            width = 5,
                              box(width = NULL,
                                  tags$script(js_upload_complete), #custom js to change upload complete text
                                  fileInput(inputId = 'ordinalInput',
                                            label = 'Browse for .csv files',
                                            accept = ".csv"),
                                  style = 'text-align: center;',
                                  div(class = 'ordBttns',
                                      actionButton(inputId = 'spearRank',
                                                   label = "Spearman's Rho"),
                                      actionButton(inputId = 'kendW',
                                                   label = "Kendall's W"),
                                      actionButton(inputId = 'tauB',
                                                   label = "Kendall's Tau"),
                                      actionButton(inputId = 'tauC',
                                                   label = "Kendall's Tau C"),
                                      actionButton(inputId = 'tauInt',
                                                   label = 'Intraclass Tau')
                                  )
                              )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 5, 
                            offset = 1,
                            style = 'padding: 0px;',
                            box(title = 'Data example', 
                                width = NULL,
                                tableOutput('expSpear'),
                                actionButton(
                                  inputId = 'spearTest',
                                  label = 'test run',
                                  style = 'float:right;'),
                                style = 'text-align: center;')),
                          column(
                            width = 5,
                            fluidRow(class = 'style_valuebox_SPEAR_cyan',
                                     column(
                                       width = 12,
                                       style = 'text-align: center;',
                                       valueBoxOutput(
                                         outputId = 'ord1',
                                         width = NULL)))
                            
                          )
                        )
)

#-------------------------------------------------------------------------------
#'*---------------------- SPEARMAN RHO SERVER LOGIC ---------------------------*
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#'*------------------- SPEARMAN RHO EXAMPLE TABLE DATA ------------------------*
#-------------------------------------------------------------------------------
# spearExp <- function(input, output) {
# output$expSpear <- function() {
#   kable(spearmanTableExample, format = 'html') %>%
#     kable_styling('basic')
#   }
# }

#-------------------------------------------------------------------------------
#'*------------------------- DEFAULT OUTPUT -----------------------------------*
#-------------------------------------------------------------------------------

#see src_functions section default out for all

#-------------------------------------------------------------------------------
#'*------------------------- CALCULATED OUTPUT --------------------------------*
#-------------------------------------------------------------------------------


ordinalRankOut <- function(input, output, data, method) {
  
  check <- help_check_ties(data)
  
  test <- ordinals(data, method)
  
  if(method != 'tauC' & method != 'tauIntra') {
    
    stat <- round(as.numeric(test$statistic), 3)
    df <- test$parameter
    p <- round(as.numeric(test$p.value), 3)
    rhoTau <- round(as.numeric(test$estimate), 3)
    W <- round(as.numeric(test$value), 3)
    
  } else if (method != 'tauIntra') {
    
    tauC <- round(test[1], 3)
    taucLower <- round(test[2], 3)
    taucUpper <- round(test[3], 3)
    
  } else if (method == 'tauIntra') {
    
    tau <- if(!is.null(test$tauIn)) {
      round(test$tauIn, 3)
    } else {
      NULL
    }
    p <- if(!is.null(test$p.value)) {
      round(test$p.value, 3)
    } else {
      NULL
    }
  }
  
  output$ord1 <- renderValueBox({
    
    valueBox(
      value = h4(
        if(method == 'spearman') {
        "Spearman's Rank Correlation Coefficient Rho"
        }
        else if(method == 'kendW') {
          "Kendall's Concordance Correlation Coefficient W"
        } 
        else if (method == 'tauB') {
          "Kendall's Tau B"
        } 
        else if( method == 'tauC') {
          "Kendall's Tau C"
        } else {
          "Intraclass Tau"
        },
      style = 'text-align: center;
              padding-top: 10px;
              font-size: 25px;'),
      
      subtitle = 
        p(
          HTML(
            paste0(
              if(method == 'spearman') {
                paste0('Rho: ', rhoTau)
              } 
              else if (method == 'kendW') {
                paste0('W: ', W)
              }
              else if(method == 'tauB') {
                paste0('Tau (A, B): ', rhoTau)
              }
              else if(method == 'tauC') {
                paste0('TauC: ', tauC)
              } 
              else if(method == 'tauIntra') {
                paste0('Tau', tags$sub('int'), ' : ', tau)
              },
              if(!is.null(df) & method != 'tauC' & method != 'tauIntra') {
                paste0(br(),'df: ', df)
              },
              if(method != 'tauC' & !is.null(p)) {
                if(method == 'tauIntra' & check) {
                  ''
                } else {
                  paste0(br(),
                         'p-value: ',
                         if(p < 0.001) {
                           '< 0.001'
                         } else {p})
                }
              } 
              else if (method == 'tauC') {
                paste0(br(),
                       'lower CI: ', taucLower, br(),
                       'upper CI: ', taucUpper)
              },
              '&emsp;', #inserting a tab (4 spaces)
              if(check & method != 'tauC' & method != 'tauIntra' & 
                 method != 'kendW') {
                circleButton(inputId = 'spear_p_value',
                             icon = icon("exclamation"),
                             size = 'xs')
              },
              if(method != 'tauC' & method != 'tauIntra') {
                paste0(br(),'test statistic: ', stat)
              }
            )
          ),
          style = 'text-align: left;
                  font-size: 25px;')
    )
  })
  
}



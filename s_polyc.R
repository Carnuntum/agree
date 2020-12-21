# source('src_functions.R')
# source('style.R')



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
                         ),
                         column(
                           width = 5,
                           box(width = NULL,
                               height = '230px',
                               #custom js to change upload complete text
                               fileInput(inputId = 'polycInput',
                                         label = 'Browse for .csv files',
                                         accept = ".csv"),
                               style = 'text-align: center;',
                               div(class = 'polycBttns',
                                   actionButton(inputId = 'polycRun',
                                                 label = "Polychoric"),
                                    actionButton(inputId = 'kruskalG',
                                                 label = 'Goodman & Kruskal Gamma'),
                                    actionButton(inputId = 'kruskalT',
                                                 label = 'Goodman & Kruskal Tau')),
                           )
                         )
                       ),
                       fluidRow(class = 'tabStyle',
                         column(
                           width = 5, 
                           offset = 1,
                           style = 'padding: 0px;',
                           uiOutput('ui_polyc1')
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

# polycDefOut <- function(inp, out) {
#   output$polyc1 <- renderValueBox({
#     
#     valueBox(value = h4("Output",
#                         style = 'text-align: center;
#                                 padding-top: 10px;
#                                 font-size: 25px;'), 
#              '')
#     
#   })
# }

#-------------------------------------------------------------------------------
#'*------------------------- CALCULATED OUTPUT --------------------------------*
#-------------------------------------------------------------------------------

polycOut <- function(input, output, data, method) {
  
  test <- polyc(data, method)
  
  test$values$Call <- NULL
  
  l_polyc <<- lapply(test$values, as.data.frame)
  
  tryCatch({
    
    if(method == 'polychoric') {
      rho <- round(test$values$rho, 3)
      rho[upper.tri(rho)] <- ''
      polycGlobalWarning <<- test$warn
    }
    else if(method == 'kruskalG') {
      rho <- round(test$values[1], 3)
      ciLow <- round(test$values[2], 3)
      ciUp <- round(test$values[3], 3)
      polycGlobalWarning <<- test$warn
    }
    else if(method == 'kruskalT') {
      rho_r <- round(test$valuesR[1], 3)
      ciLow_r <- round(test$valuesR[2], 3)
      ciUp_r <- round(test$valuesR[3], 3)
      
      rho_c <- round(test$valuesC[1], 3)
      ciLow_c <- round(test$valuesC[2], 3)
      ciUp_c <- round(test$valuesC[3], 3)
    }
    
  }, error = function(e) {
    print(e)
  })
  
  
  
  
  
  output$polyc1 <- renderValueBox({
    
    tryCatch({
      valueBox(value = h4(
        if(method == 'polychoric') {
          "Polychoric Correlation Rho"
          }
        else if(method == 'kruskalG') {
          "Goodman & Kruskal's Gamma"
        }
        else if(method == 'kruskalT') {
          "Goodman & Kruskal's Tau"
        },
        style = 'text-align: center;
                padding-top: 10px;
                font-size: 25px;'),
        
        subtitle = p(HTML(
          paste0(
            if(method == 'polychoric') {
              if(!is.matrix(rho)) {
                paste0('Rho: ', rho,
                       br(), br(),
                       'Tau Thresholds: ', br(),
                       kableExtra::kable(cbind('row_thres' = test$values$tau.row,
                                   'col_thres' = test$values$tau.col),
                             'html') %>% 
                         kableExtra::kable_styling('basic',
                                       font_size = 18)
                       )
              }
              else if (is.matrix(rho)) {
                paste0(
                  kableExtra::kable(rho, format = 'html') %>%
                    kableExtra::kable_styling(bootstrap_options = 'basic',
                                  font_size = 18),
                  br(),
                  p('Tau Thresholds: ', style = 'font-size: 18px;'),
                  br(),
                  kableExtra::kable(round(test$values$tau, 3), 'html') %>%
                    kableExtra::kable_styling(bootstrap_options = 'basic',
                                  font_size = 18),
                  
                  
                  if (!is.null(test$warn)) {
                    circleButton(inputId = 'polycWarn',
                                 icon = icon("exclamation"),
                                 size = 's')
                  }
                )
              }
            }
            
            else if (method == 'kruskalG') {
              paste0('Gamma: ', rho, br(),
                     'lower CI: ', ciLow, br(),
                     'upper CI: ', ciUp)
            }
            else if (method == 'kruskalT') {
              paste0(
                'Direction row', br(), br(),
                'Tau: ', rho_r, br(),
                'lower CI: ', ciLow_r,br(),
                'upper CI: ', ciUp_r, 
                br(),br(),
                'Direction column', br(), br(),
                'Tau: ', rho_c, br(),
                'lower CI: ', ciLow_c, br(),
                'upper CI: ', ciUp_c
              )
            }
          )
        ),
        div(
          downloadButton(outputId = 'polycFullDown',
                         label = 'Full Results'),
          style = 'text-align: center;'
        ), 
        style = 'text-align: left;
                      font-size: 18px;')
      )}, 
      error = function(e) {
        print(paste('error occured: ', e))
        polycErrOut(output, test)
      },
      warning = function(w) {
        print(conditionMessage(w))
        polycErrOut(output, test)
      })
  })
  
}


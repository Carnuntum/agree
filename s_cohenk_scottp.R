#-------------------------------------------------------------------------------
#'*------------------------- COHENS KAPPA TABITEM -----------------------------*
#-------------------------------------------------------------------------------
source('src_functions.R')
source('style.R')


cohenk <- tabItem(tabName = 'cohenk', useShinyjs(),
                  
    fluidRow(
      column(
        width = 10,
        offset = 1,
        style = 'padding-left: 0px; padding-right: -5px;',
        box(
          width = NULL,
          h3(id = 'kappaDocum',
             "Cohen's Kappa And Scott's Pi For 2 Raters And N Categories"),
          style = 'text-align:center; padding: 30px;'
        )
    )),
    fluidRow(
      style = 'text-align: center;',
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
            fileInput(inputId = 'kappaInput',
                      label = 'Browse for .csv file'),
            div(class = 'kappaBttns',
                actionButton(inputId = 'kappa',
                             label = "Cohen's Kappa"),
                actionButton(inputId = 'spi',
                             label = "Scott's Pi"),
                actionButton(inputId = 'fleissKappa',
                             label = "Fleiss' Kappa")
                ), style = 'text-align: center;'
            )
        )
    ),
    fluidRow(class = 'tabStyle',
      column(
        width = 5, 
        offset = 1,
        style = 'padding: 0px;',
        uiOutput('ui_kappa')
      ),
      column(
        width = 5,
        fluidRow(class = 'style_valuebox_KAPPA_cyan',
                 column(
                   width = 12,
                   style = 'text-align: center;',
                   valueBoxOutput(
                     outputId = 'kappa1',
                     width = NULL)))
        
        
      )
    )
  )

#-------------------------------------------------------------------------------
#'*-------------------------- KAPPA SERVER LOGIC --------------------------------*
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#'*-------------------- KAPPA EXAMPLE TABLE DATA ------------------------------*
#-------------------------------------------------------------------------------
# kappaExampleTable <- function(inp, out) {
#   output$expKappa <- function() {
#     kable(kappaTableExample, format = 'html') %>%
#       kable_styling('basic')
#   }
# }


#-------------------------------------------------------------------------------
#'*-------------- DEFAULT INFORMATION FOR KAPPA OUTPUT BOXES ------------------*
#-------------------------------------------------------------------------------
# kappaOutDefault <- function(inp, out) {
#   output$kappa1 <- renderValueBox({
#     if (is.null(input$kappaInput)) {
#       valueBox(value = h5(''), subtitle = "Cohen's Kappa")
#     }
#   })
# }

#-------------------------------------------------------------------------------
#'*------------------ CALCULATED OUTPUT FOR KAPPA -----------------------------*
#-------------------------------------------------------------------------------

kappaOut <- function(input, output, data, method) {
  
  test <- kap_scott_fleiss(data, method)
  
  l_kappa <<- lapply(test$values, as.data.frame)
  
  output$kappa1 <- renderValueBox({
    tryCatch({
      valueBox(
        subtitle =
          p(HTML(paste0(
            if (method == 'kappa') {
              paste0(
                'K: ',
                round(as.numeric(test$values$value), 3),
                br(),
                'z-value: ',
                round(as.numeric(test$values$statistic), 3),
                br(),
                
                if (test$values$p.value < 0.001) {
                  'p-value: < 0.001'
                }
                else {
                  paste0('p-value: ',
                         round(as.numeric(test$values$p.value), 3))
                }
              )
            }
            else if (method == 'spi') {
              paste0('Pi: ', round(test$values$est, 3))
            }
            else if (method == 'fleissKappa') {
              paste0(
                "Fleiss' K: ",
                round(test$values$value, 3),
                br(),
                'z-value: ',
                round(test$values$statistic, 3),
                br(),
                if (test$values$p.value < 0.001) {
                  'p-value: < 0.001'
                } else {
                  paste0('p-value: ', round(test$values$p.value, 3))
                }
              )
            }
            
          )),
          div(
            downloadButton(outputId = 'kappaFullDown',
                           label = 'Full Results'),
            style = 'text-align: center;'
          ),
          style = 'font-size: 25px; text-align:center;'),
        value = 
        p(HTML(
          if (method == 'kappa') {
            "Cohen's Kappa"
          }
          else if (method == 'spi') {
            "Scott's Pi"
          }
          else if (method == 'fleissKappa') {
            "Fleiss' Kappa"
          }
        ), style = 'color: white;')
      )
    }, error = function(e) {
      print(e)
      output$kappa1 <- renderValueBox({
        valueBox(value = h5(''), subtitle = "Invalid Data")
      })
    }, warning = function(w) {
      print(w)
      output$kappa1 <- renderValueBox({
        valueBox(value = h5(''), subtitle = "Invalid Data")
      })
    })
  })
}

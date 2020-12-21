#-------------------------------------------------------------------------------
#'*----------------------------- CHI TABITEM ----------------------------------*
#-------------------------------------------------------------------------------

chi <- tabItem(tabName = 'chi', 
               useShinyjs(), 
               fluidRow(
                 column(
                   width = 10,
                   offset = 1,
                   style = 'padding-left: 0px; padding-right: -5px;',
                   box(
                     width = NULL,
                     style = 'text-align:center; padding: 30px;',
                     h3("Chi Square Test")
                   )
                )
               ),
               fluidRow(class = 'chiFileInput',
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
                       fileInput(inputId = 'chiInput',
                                 label = 'Browse for .csv files'),
                       actionButton(inputId = 'chiRun',
                                    label = 'calculate')),
                   style = 'text-align: center;')
               ),
               fluidRow(
                 class = 'tabStyle',
                 column(
                   width = 5, 
                   offset = 1,
                   style = 'padding: 0px;',
                   uiOutput('ui_chi')
                   ),
                 column(
                   width = 5,
                   fluidRow(class = 'style_valuebox_CHI_cyan',
                            column(
                              width = 12,
                              style = 'text-align: center;',
                              valueBoxOutput(
                                outputId = 'chi1',
                                width = NULL)))
                   
                 )
               )
)

#-------------------------------------------------------------------------------
#'*-------------------------- CHI SERVER LOGIC --------------------------------*
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#'*-------------------- CHI EXAMPLE TABLE DATA --------------------------------*
#-------------------------------------------------------------------------------
# chiExampleTable <- function(input, output) {
#   output$expChi <- function() {
#     kable(chiTableExp, format = 'html') %>%
#       kable_styling('basic')
#   }
# }

#-------------------------------------------------------------------------------
#'*---------------- DEFAULT INFORMATION FOR CHI OUTPUT BOXES ------------------*
#-------------------------------------------------------------------------------
# chiOutDefault <- function(inp, out) {
#   output$chiUncor <- renderValueBox({
#     if (is.null(input$chiInput)) {
#       valueBox(value = h5('Statistic uncorrected'), '')
#     }
#   })
#   output$chiCor <- renderValueBox({
#     if (is.null(input$chiInput)) {
#       valueBox(value = h5('Statistic corrected'), '')
#     }
#   })
#   
#   output$contingencyCoeff <- renderValueBox({
#     if (is.null(input$chiInput)) {
#       valueBox(value = h5('Contingency Coefficient C'), '')
#     }
#   })
# }



#-------------------------------------------------------------------------------
#'*------------------ CALCULATED OUTPUT FOR CHI TEST --------------------------*
#-------------------------------------------------------------------------------
#chi output for corrected test
chiOut <- function(input, output, data) {
  
  tryCatch({
    test_uncorr <- chisq.test(data, correct = F)
    test_corr <- chisq.test(data, correct = T)
    chi_uncorr_val <- test_uncorr$statistic
    chi_corr_val <- test_corr$statistic
    chi_uncorr_p <- test_uncorr$p.value
    chi_corr_p <- test_corr$p.value
    chi_uncorr_par <- test_uncorr$parameter
    chi_corr_par <- test_corr$parameter
    coeffC <- DescTools::ContCoef(data)
    
    d_chi <- t(
      data.frame(
        chi_uncorrected = chi_uncorr_val,
        chi_uncorrected_df = chi_uncorr_par,
        chi_uncorrected_p = chi_uncorr_p,
        chi_corrected = chi_corr_val,
        chi_corrected_df = chi_corr_par,
        chi_corrected_p = chi_corr_p,
        contingency_coeff = coeffC
      )
    )
    
    l_chi <<- as.list(d_chi)
    
    output$chi1 <- renderValueBox({
      tryCatch({
        valueBox(
          value = p(HTML(
            kableExtra::kable(round(d_chi, 3), format = 'html') %>% 
              kableExtra::kable_styling('basic'),
            
          ),
          div(
            downloadButton(outputId = 'chiFullDown',
                           label = 'Full Results'),
            style = 'text-align: center;'
          )),
          subtitle = ''
        )
      }, error = function(e) {
        print(e)
      }, warning = function(w) {
        print(w)
        invalid_data(output, 'chi1')
      })
    })
    
  }, error = function(e) {
    print(e)
  }, warning = function(w) {
    print(w)
  })
  
}





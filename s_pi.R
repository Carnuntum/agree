#-------------------------------------------------------------------------------
#'*----------------------------- PI TABITEM ----------------------------------*
#-------------------------------------------------------------------------------

kappa_pi <- tabItem(tabName = 'kappa_pi', 
               useShinyjs(), 
               fluidRow(
                 column(
                   width = 10,
                   offset = 1,
                   style = 'padding-left: 0px; padding-right: -5px;',
                   box(
                     id = 'piDocum',
                     width = NULL,
                     style = measure_title_style,
                     h3("Scott's Pi")
                   ),
                   hidden(
                     div(id = 'piDocumBox',
                         fluidRow(class = 'documRow',
                                  column(width = 12,
                                         offset = 0,
                                         box(title = pi_docum_text,
                                             width = NULL,
                                             style = 'text-align:center; padding: 0;')
                                  )
                         )
                     )
                   )
                )
               ),
               fluidRow(class = 'piFileInput',
                        column(
                          width = 5,
                          offset = 1,
                          fluidRow(
                            box(
                              width = NULL,
                              height = '105px',
                              p(file_upload_text),
                              style = centerText
                            )
                          ),
                          fluidRow(
                            box(
                              width = NULL,
                              height = '105px',
                              p(file_struct_text),
                              look_down,
                              style = centerText
                            )
                          )
                        ),
                 column(
                   width = 5,
                   box(width = NULL,
                       fileInput(inputId = 'piInput',
                                 label = 'Browse for .csv files'),
                       h5('Choose weighting method'),
                       div(class = 'checkbox_button_css', 
                           checkboxGroupButtons(inputId = 'pi_weight',
                                                choices = c('unweighted',
                                                            'linear',
                                                            'quadratic'))),
                       
                       actionButton(inputId = 'piRun',
                                    label = 'calculate')
                       ),
                   style = centerText)
               ),
               fluidRow(
                 class = 'tabStyle',
                 column(
                   width = 5, 
                   offset = 1,
                   style = 'padding: 0px;',
                   uiOutput('ui_pi')
                   ),
                 column(
                   width = 5,
                   fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                            column(
                              width = 12,
                              style = centerText,
                              shinyBS::popify(valueBoxOutput(outputId = 'pi1', width = NULL), 
                                              title = 'What means what',
                                              content = paste0('<li>', names(pi_output_description),
                                                               ' = ',
                                                               as.character(pi_output_description), '</li>',
                                                               br()),
                                              placement = 'left'
                              )
                              ))
                   
                 )
               )
)



#-------------------------------------------------------------------------------
#'*------------------ CALCULATED OUTPUT FOR PI TEST --------------------------*
#-------------------------------------------------------------------------------
#pi output for corrected test
piOut <- function(input, output, data) {
  
  tryCatch({
    choice <- input$pi_weight
    
    vals_pi <- rel::spi(data, if(!is.null(choice)) {weight = choice} else {
      weight = 'unweighted'
    })
    
    vals_pi$call <- NULL
    l_pi <<- lapply(vals_pi, as.data.frame)
    
    d_pi <- as.list(vals_pi)
    
    #at 3 only data frames -> cannot be shown in table
    d_pi <- as.data.frame(d_pi[1:(length(d_pi) - 3)])
    d_pi <- t(as.data.frame(d_pi))
    
    output$pi1 <- renderValueBox({
      tryCatch({
        valueBox(
          subtitle = p(HTML(
            kableExtra::kable(d_pi, format = 'html') %>% 
              kableExtra::kable_styling('basic')
          ),
          div(
            downloadButton(outputId = 'piFullDown',
                           label = 'Full Results'),
            style = centerText
          )),
          value = ''
        )
      }, error = function(e) {
        print(e)
      }, warning = function(w) {
        print(w)
        invalid_data(output, 'pi1')
      })
    })
    
  }, error = function(e) {
    print(e)
  }, warning = function(w) {
    print(w)
  })
  
}





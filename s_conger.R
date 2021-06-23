#source('style.R')

kappa_conger <- tabItem(tabName = 'kappa_conger', 
                    useShinyjs(), 
                    fluidRow(
                      column(
                        width = 10,
                        offset = 1,
                        style = 'padding-left: 0px; padding-right: -5px;',
                        box(
                          id = 'congerDocum',
                          width = NULL,
                          style = measure_title_style,
                          h3("Conger's Kappa")
                        ),
                        hidden(
                          div(id = 'congerDocumBox',
                              fluidRow(class = 'documRow',
                                       column(width = 12,
                                              offset = 0,
                                              box(title = conger_docum_text,
                                                  width = NULL,
                                                  style = 'text-align:center; padding: 0;')
                                       )
                              )
                          )
                        )
                      )
                    ),
                    fluidRow(class = 'congerFileInput',
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
                                   fileInput(inputId = 'congerInput',
                                             label = 'Browse for .csv files'),
                                   h5('Choose weighting method'),
                                   div(class = 'selectInputStyle',
                                       selectInput(inputId = 'conger_weight',
                                                   label = '',
                                                   choices = c('unweighted',
                                                               'linear',
                                                               'quadratic',
                                                               'ordinal',
                                                               'radical',
                                                               'ratio',
                                                               'circular',
                                                               'bipolar')
                                       ),
                                       style = centerText),
                                   
                                   actionButton(inputId = 'congerRun',
                                                label = 'calculate')
                               ),
                               style = 'text-align: center;')
                    ),
                    fluidRow(
                      class = 'tabStyle',
                      column(
                        width = 5, 
                        offset = 1,
                        style = 'padding: 0px;',
                        uiOutput('ui_conger')
                      ),
                      column(width = 5,
                             shinyWidgets::dropMenu(
                               div(id = 'congerDrop',
                                   fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                                            column(
                                              width = 12,
                                              valueBoxOutput(outputId = 'conger', width = NULL)
                                            )
                                   )
                               ),
                               HTML(kableExtra::kable(t(conger_output_description)) %>% 
                                      kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri')),
                               trigger = 'mouseenter',
                               theme = 'translucent',
                               placement = 'left-start')
                      )
                    )
)



#-------------------------------------------------------------------------------
#'*------------------ CALCULATED OUTPUT FOR PI TEST --------------------------*
#-------------------------------------------------------------------------------
#pi output for corrected test
congerOut <- function(input, output, data) {
  
  tryCatch({
    choice <- input$conger_weight
    
    vals_conger <- list('vals' = warning_handler(irrCAC::conger.kappa.raw(data, if(!is.null(choice)) {weights = choice} else {
      weights = 'unweighted'
    })),
    'warn' = msg)
    
    vals_conger$vals$call <- NULL
    l_conger <<- lapply(vals_conger$vals, as.data.frame)
    
    d_conger <- as.list(vals_conger$vals)
    
    #at 3 only data frames -> cannot be shown in table
    d_conger <- as.data.frame(d_conger[1:(length(d_conger) - 3)])
    d_conger <- t(as.data.frame(d_conger))
    
    output$conger <- renderValueBox({
      tryCatch({
        valueBox(
          subtitle = p(HTML(
            kableExtra::kable(d_conger, format = 'html') %>% 
              kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri')
          ),
          div(
            if(!is.null(msg)) {
              p(HTML(paste0(
                circleButton(inputId = 'warningButton',
                             icon = icon("exclamation"),
                             size = 's'),
                br()
              )))
            },
            style = centerText
          ),
          div(
            downloadButton(outputId = 'congerFullDown',
                           label = 'Full Results'),
            style = 'text-align: center;'
          )),
          value = ''
        )
      }, error = function(e) {
        invalid_data(output, 'conger')
        print(e)
      }, warning = function(w) {
        print(w)
        invalid_data(output, 'conger')
      })
    })
    
  }, error = function(e) {
    invalid_data(output, 'conger')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'conger')
    print(w)
  })
  
}

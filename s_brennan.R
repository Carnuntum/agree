
kappa_brennan <- tabItem(
  tabName = 'kappa_brennan',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    id = 'brennanDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("Brennan & Prediger's Kappa")
                  ),
                  hidden(
                    div(id = 'brennanDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = brennan_docum_text,
                                            width = NULL,
                                            style = 'text-align:center; padding: 0;')
                                 )
                        )
                    )
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
    )
    ,
    column(
      width = 5,
      box(
        width = NULL,
        fileInput(inputId = 'brennanInput',
                  label = 'Browse for .csv files'),
        h5('Choose weighting method'),
        div(class = 'selectInputStyle',
            selectInput(inputId = 'brennan_weight',
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
        actionButton(
          inputId = 'brennanRun',
          label = 'calculate'
        ),
        style = centerText
      )
    )
  ),
  fluidRow(
    class = 'tabStyle',
    column(
      width = 5,
      offset = 1,
      style = 'padding: 0px;',
      uiOutput('ui_brennan')
    ),
    
    column(width = 5,
           shinyWidgets::dropMenu(
             div(id = 'brennanDrop',
                 fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                          column(
                            width = 12,
                            valueBoxOutput(outputId = 'brennan', width = NULL)
                          )
                 )
             ),
             HTML(kableExtra::kable(t(brennan_output_description)) %>% 
                    kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri')),
             trigger = 'mouseenter',
             theme = 'translucent',
             placement = 'left-start')
    )
  )
)




#-------------------------------------------------------------------------------
#'*--------------- CALCULATED OUTPUT FOR PERCENT AGREEMENT --------------------*
#-------------------------------------------------------------------------------
#chi output for corrected test
brennanOut <- function(input, output, data) {
  
  tryCatch({
    
    choice <- input$brennan_weight
    
    vals_brennan <- list('vals' = warning_handler(irrCAC::bp.coeff.raw(data,
                                                                       if(!is.null(choice)) {weights = choice} 
                                                                       else { 
                                                                         weights = 'unweighted'
                                                                         }
                                                                       )$est
                                                  ),
                         'warn' = msg)
    
    l_brennan <<- lapply(vals_brennan$vals, as.data.frame)
    
    class(vals_brennan$vals) <- 'list'
    
    d_brennan <- t(as.data.frame(vals_brennan$vals))
    
    output$brennan <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_brennan, format = 'html') %>% 
            kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri'),
          
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
          downloadButton(outputId = 'brennanFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'brennan')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'brennan')
    print(w)
  })
}

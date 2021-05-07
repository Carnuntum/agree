
kappa_gwet <- tabItem(
  tabName = 'kappa_gwet',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    id = 'gwetDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("Gwet's Agreement Coefficient")
                  ),
                  hidden(
                    div(id = 'gwetDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = gwet_docum_text,
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
        fileInput(inputId = 'gwetInput',
                  label = 'Browse for .csv files'),
        h5('Choose weighting method'),
        div(class = 'selectInputStyle',
            selectInput(inputId = 'gwet_weight',
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
          inputId = 'gwetRun',
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
      uiOutput('ui_gwet')
    ),
    
    column(width = 5,
           fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                    column(
                      width = 12,
                      shinyBS::popify(valueBoxOutput(outputId = 'gwet', width = NULL), 
                                      title = 'What means what',
                                      content = paste0('<li>', names(gwet_output_description),
                                                       ' = ',
                                                       as.character(gwet_output_description), '</li>',
                                                       br()),
                                      placement = 'left'
                      )
                    )
           )
    )
  )
)




#-------------------------------------------------------------------------------
#'*--------------- CALCULATED OUTPUT FOR PERCENT AGREEMENT --------------------*
#-------------------------------------------------------------------------------
#chi output for corrected test
gwetOut <- function(input, output, data) {
  
  tryCatch({
    
    choice <- input$gwet_weight
    
    vals_gwet <- list('vals' = warning_handler(irrCAC::gwet.ac1.raw(data,
                                                                       if(!is.null(choice)) {weights = choice} 
                                                                       else { 
                                                                         weights = 'unweighted'
                                                                       }
    )$est
    ),
    'warn' = msg)
    
    l_gwet <<- lapply(vals_gwet$vals, as.data.frame)
    
    class(vals_gwet$vals) <- 'list'
    
    d_gwet <- t(as.data.frame(vals_gwet$vals))
    
    output$gwet <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_gwet, format = 'html') %>% 
            kableExtra::kable_styling('basic'),
          
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
          downloadButton(outputId = 'gwetFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'gwet')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'gwet')
    print(w)
  })
}
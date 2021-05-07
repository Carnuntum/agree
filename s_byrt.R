
kappa_byrt <- tabItem(
  tabName = 'kappa_byrt',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    id = 'byrtDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("Byrt's Kappa")
                  ),
                  hidden(
                    div(id = 'byrtDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = byrt_docum_text,
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
        fileInput(inputId = 'byrtInput',
                  label = 'Browse for .csv files'),
        actionButton(
          inputId = 'byrtRun',
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
      uiOutput('ui_byrt')
    ),
    
    column(width = 5,
           fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                    column(
                      width = 12,
                      shinyBS::popify(valueBoxOutput(outputId = 'byrt', width = NULL), 
                                      title = 'What means what',
                                      content = paste0('<li>', names(byrt_output_description),
                                                       ' = ',
                                                       as.character(byrt_output_description), '</li>',
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
byrtOut <- function(input, output, data) {
  
  tryCatch({
    
    vals_byrt <- list('vals' = warning_handler(byrtKappa(data)),
                      'warn' = msg)
    
    l_byrt <<- lapply(vals_byrt$vals, as.data.frame)
    
    d_byrt <- t(as.data.frame(vals_byrt$vals))
    
    output$byrt <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_byrt, format = 'html') %>% 
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
          downloadButton(outputId = 'byrtFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'byrt')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'byrt')
    print(w)
  })
}


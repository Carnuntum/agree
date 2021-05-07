
kappa_aickin <- tabItem(
  tabName = 'kappa_aickin',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    id = 'aickinDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("Aickin's Alpha")
                  ),
                  hidden(
                    div(id = 'aickinDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = aickin_docum_text,
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
        fileInput(inputId = 'aickinInput',
                  label = 'Browse for .csv files'),
        actionButton(
          inputId = 'aickinRun',
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
      uiOutput('ui_aickin')
    ),
    
    column(width = 5,
           fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                    column(
                      width = 12,
                      shinyBS::popify(valueBoxOutput(outputId = 'aickin', width = NULL), 
                                      title = 'What means what',
                                      content = paste0('<li>', names(aickin_output_description),
                                                       ' = ',
                                                       as.character(aickin_output_description), '</li>',
                                                       br()),
                                      placement = 'left'
                      )
                    )
           )
    )
  )
)




#-------------------------------------------------------------------------------
#'*----------------- CALCULATED OUTPUT FOR AICKINS ALPHA ----------------------*
#-------------------------------------------------------------------------------
#chi output for corrected test
aickinOut <- function(input, output, data) {
  
  tryCatch({
    
    vals_aickin <- list('vals' = warning_handler(aickinAlpha(data)),
                        'warn' = msg)
    
    l_aickin <<- lapply(vals_aickin$vals, as.data.frame)
    
    class(vals_aickin$vals) <- 'list'
    
    d_aickin <- t(as.data.frame(vals_aickin$vals))
    
    output$aickin <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_aickin, format = 'html') %>% 
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
          downloadButton(outputId = 'aickinFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'aickin')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'aickin')
    print(w)
  })
}
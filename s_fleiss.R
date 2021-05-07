source('style.R')
source('text.R')

kappa_fleiss <- tabItem(
  tabName = 'kappa_fleiss',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    id = 'fleissDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("Fleiss' Kappa")
                  ),
                  hidden(
                    div(id = 'fleissDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = fleiss_docum_text,
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
    )
    ,
    column(
      width = 5,
      box(
        width = NULL,
        height = '230px',
        fileInput(inputId = 'fleissInput',
                  label = 'Browse for .csv files'),
        actionButton(
          inputId = 'fleissRun',
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
      uiOutput('ui_fleiss')
    ),
    
    column(width = 5,
           fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                    column(
                      width = 12,
                      shinyBS::popify(valueBoxOutput(outputId = 'fleiss', width = NULL), 
                                      title = 'What means what',
                                      content = paste0('<li>', names(fleiss_output_description),
                                                       ' = ',
                                                       as.character(fleiss_output_description), '</li>',
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
fleissOut <- function(input, output, data) {
  
  tryCatch({
    
    vals_fleiss <- list('vals' = warning_handler(irr::kappam.fleiss(data, exact = F, detail = F)),
                       'warn' = msg)
    
    l_fleiss <<- lapply(vals_fleiss$vals, as.data.frame)
    
    class(vals_fleiss$vals) <- 'list'
    
    d_fleiss <- t(as.data.frame(vals_fleiss$vals))
    
    output$fleiss <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_fleiss, format = 'html') %>% 
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
          downloadButton(outputId = 'fleissFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'fleiss')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'fleiss')
    print(w)
  })
}

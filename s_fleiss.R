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
        fileInput(inputId = 'fleissInput',
                  label = 'Browse for .csv files'),
        h5('Choose weighting method'),
        div(class = 'selectInputStyle',
            selectInput(inputId = 'fleiss_weight',
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
            shinyWidgets::dropMenu(
              div(id = 'fleissDrop',
                  fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                           column(
                             width = 12,
                             valueBoxOutput(outputId = 'fleiss', width = NULL)
                           )
                  )
              ),
              HTML(kableExtra::kable(t(fleiss_output_description)) %>% 
                     kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri')),
              trigger = 'click',
              theme = 'translucent',
              placement = 'left-start')
     )
  )
)




#-------------------------------------------------------------------------------
#'*--------------- CALCULATED OUTPUT FOR PERCENT AGREEMENT --------------------*
#-------------------------------------------------------------------------------
#chi output for corrected test
fleissOut <- function(input, output, data) {
  
  tryCatch({
    
    choice <<- input$fleiss_weight
    
    vals_fleiss <- list('vals' = warning_handler(fleiss(data, choice)),
                       'warn' = msg)
    
    l_fleiss <<- lapply(vals_fleiss$vals, as.data.frame)
    
    d_fleiss <- (as.data.frame(vals_fleiss$vals))
    
    if(!is.nan(vals_fleiss$vals$f.est)) {
      ci <- warning_handler(makeCi(data, bfun_fleiss))
      
      d_fleiss$lb <- ci[1]
      d_fleiss$ub <- ci[2]
    }
    
    d_fleiss <- t(d_fleiss)
    
    output$fleiss <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_fleiss, format = 'html') %>% 
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

bfun_fleiss <- function(d, i) {
  dat <- d[i,]
  return(fleiss(dat, choice)$f.est)
}

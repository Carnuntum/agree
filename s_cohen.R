#source('src_functions.R')
#source('style.R')

kappa_cohen <- tabItem(
  tabName = 'kappa_cohen',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px;',
                  box(
                    id = 'cohenDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("Cohen's Kappa")
                  ),
                  hidden(
                    div(id = 'cohenDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = cohen_docum_text,
                                            width = NULL,
                                            style = measure_title_style)
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
        fileInput(inputId = 'cohenInput',
                  label = 'Browse for .csv files'),
        h5('Choose weighting method'),
        div(class = 'selectInputStyle',
            selectInput(inputId = 'cohen_weight',
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
          inputId = 'cohenRun',
          label = 'calculate'
        ),
        style = 'text-align: center;'
      )
    )
  ),
  fluidRow(
    class = 'tabStyle',
    column(
      width = 5,
      offset = 1,
      style = 'padding: 0px;',
      uiOutput('ui_cohen')
    ),
    
    column(width = 5,
           shinyWidgets::dropMenu(
             div(id = 'cohenDrop',
                 fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                          column(
                            width = 12,
                            valueBoxOutput(outputId = 'cohen', width = NULL)
                          )
                 )
             ),
             HTML(kableExtra::kable(t(cohen_output_description)) %>% 
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
cohenOut <- function(input, output, data) {
  
  tryCatch({
    choice <- input$cohen_weight
      
    vals_cohen <- list('vals' = warning_handler(cohenKappa(data,
                                                            if(!is.null(choice)) {weight = choice}
                                                            else {weight = 'unweighted'})),
                       'warn' = msg)
    
    l_cohen <<- lapply(vals_cohen$vals, as.data.frame)
    
    class(vals_cohen$vals) <- 'list'
    
    d_cohen <- t(as.data.frame(vals_cohen$vals))
    
    output$cohen <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_cohen, format = 'html') %>% 
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
          downloadButton(outputId = 'cohenFullDown',
                         label = 'Full Results'),
          style = 'text-align: center;'
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'cohen')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'cohen')
    print(w)
  })
}





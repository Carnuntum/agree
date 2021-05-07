#source('src_functions.R')
#source('style.R')

kappa_cohen <- tabItem(
  tabName = 'kappa_cohen',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
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
        height = '230px',
        fileInput(inputId = 'cohenInput',
                  label = 'Browse for .csv files'),
        h5('Choose weighting method'),
        div(class = 'checkbox_button_css', 
            checkboxGroupButtons(inputId = 'cohen_weight',
                                 choices = c('unweighted',
                                             'equal',
                                             'squared'))),
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
           fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                    column(
                      width = 12,
                      shinyBS::popify(valueBoxOutput(outputId = 'cohen', width = NULL), 
                                      title = 'What means what',
                                      content = paste0('<li>', names(cohen_output_description),
                                                       ' = ',
                                                       as.character(cohen_output_description), '</li>',
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
cohenOut <- function(input, output, data) {
  
  tryCatch({
    choice <- input$cohen_weight
      
    vals_cohen <- list('vals' = warning_handler(irr::kappa2(data,
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





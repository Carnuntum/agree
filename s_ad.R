
other_ad <- tabItem(
  tabName = 'other_ad',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    id = 'adDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("Agreement Coefficient d")
                  ),
                  hidden(
                    div(id = 'adDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = ad_docum_text,
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
        fileInput(inputId = 'adInput',
                  label = 'Browse for .csv files'),
        h5('Number of rated categories'),
        numericInput(
          inputId = 'adNumInput',
          value = 1,
          label = '',
          min = 1
        ),
        actionButton(
          inputId = 'adRun',
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
      uiOutput('ui_ad')
    ),
    
    column(width = 5,
           fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                    column(
                      width = 12,
                      shinyBS::popify(valueBoxOutput(outputId = 'ad', width = NULL), 
                                      title = 'What means what',
                                      content = paste0('<li>', names(ad_output_description),
                                                       ' = ',
                                                       as.character(ad_output_description), '</li>',
                                                       br()),
                                      placement = 'left'
                      )
                    )
           )
    )
  )
)




#-------------------------------------------------------------------------------
#'*------------------------------- AD INDEX -----------------------------------*
#-------------------------------------------------------------------------------
#chi output for corrected test
adOut <- function(input, output, data) {
  
  tryCatch({
    
    data <- na.omit(data)
    nCat <- input$adNumInput
    
    ad <- warning_handler(adCoeff(data, nCat))
    adCrit <- warning_handler(adCritical(data, nCat))
    
    vals_ad <- list('vals' = list('d' = ad, 'd-crit' = adCrit,
                                  'sign.' = if(ad > adCrit) {'true'} else {'false'}),
                    'warn' = msg)
    
    l_ad <<- lapply(vals_ad$vals, as.data.frame)
    
    d_ad <- t(as.data.frame(vals_ad$vals))
    
    output$ad <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_ad, format = 'html') %>% 
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
          downloadButton(outputId = 'adFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'ad')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'ad')
    print(w)
  })
}
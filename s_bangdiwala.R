
other_bangdiwala <- tabItem(
  tabName = 'other_bangdiwala',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    id = 'bangdiwalaDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("Bangdiwala's B")
                  ),
                  hidden(
                    div(id = 'bangdiwalaDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = bangdiwala_docum_text,
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
        fileInput(inputId = 'bangdiwalaInput',
                  label = 'Browse for .csv files'),
        h5('Choose weighting method'),
        div(class = 'selectInputStyle',
            selectInput(inputId = 'bangdiwala_weight',
                        label = '',
                        choices = c('linear',
                                    'quadratic',
                                    'ordinal',
                                    'radical',
                                    'ratio',
                                    'circular',
                                    'bipolar')
            ),
            style = centerText),
        actionButton(
          inputId = 'bangdiwalaRun',
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
      uiOutput('ui_bangdiwala')
    ),
    
    column(width = 5,
           fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                    column(
                      width = 12,
                      shinyBS::popify(valueBoxOutput(outputId = 'bangdiwala', width = NULL), 
                                      title = 'What means what',
                                      content = paste0('<li>', names(bangdiwala_output_description),
                                                       ' = ',
                                                       as.character(bangdiwala_output_description), '</li>',
                                                       br()),
                                      placement = 'left'
                      )
                    )
           )
    )
  ),
  shinyjs::hidden(
    div(id = 'div_plot_bangdiwala',
        box(
          title = 'Agreement Plot',
          width = NULL,
          plotOutput('plot_bangdiwala', width = '100%', height = '600px'),
          downloadButton('down_plot_bangdiwala', style = 'margin: 20px;')
        ),
        style = centerText
    )
  )
)




#-------------------------------------------------------------------------------
#'*--------------- CALCULATED OUTPUT FOR PERCENT AGREEMENT --------------------*
#-------------------------------------------------------------------------------
#chi output for corrected test
bangdiwalaOut <- function(input, output, data) {
  
  tryCatch({
    
    choice <- input$bangdiwala_weight
    
    data <- na.omit(data)
    
    w <- warning_handler(do.call(paste0(choice, '.weights', collapse = ''), args = list(1:max(data)))[1,])
    
    bangdi_data <<- table(data[,1], data[,2])
    
    vals_bangdiwala <- list('vals' = warning_handler(vcd::agreementplot(bangdi_data, weights = w, return_grob = T)),
                            'warn' = msg)
    
    bangdiwala_plot <<- attributes(vals_bangdiwala$vals)$grob
    
    l_bangdiwala <<- lapply(vals_bangdiwala$vals, as.data.frame)
    
    d_bangdiwala <- t(as.data.frame(vals_bangdiwala$vals))
    
    shinyjs::show('div_plot_bangdiwala', anim = T)
    output$plot_bangdiwala <- renderPlot({
      grid::grid.draw(bangdiwala_plot)
    })
    
    bangdiwala_weights <- data.frame(w = d_bangdiwala[3,])
    colnames(bangdiwala_weights) <- paste0('weights - ', choice, collapse = '')
    
    output$bangdiwala <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_bangdiwala[1:2,1], format = 'html') %>% 
            kableExtra::kable_styling('basic'),
          
          kableExtra::kable(bangdiwala_weights, format = 'html') %>% 
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
          downloadButton(outputId = 'bangdiwalaFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'bangdiwala')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'bangdiwala')
    print(w)
  })
}







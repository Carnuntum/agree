
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
           shinyWidgets::dropMenu(
             div(id = 'bangdiwalaDrop',
                 fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                          column(
                            width = 12,
                            valueBoxOutput(outputId = 'bangdiwala', width = NULL)
                          )
                 )
             ),
             HTML(kableExtra::kable(t(bangdiwala_output_description)) %>% 
                    kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri')),
             trigger = 'mouseenter',
             theme = 'translucent',
             placement = 'left-start')
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
    
    if(ncol(data) > 2) {warning_handler(stop('computation is only possible for 2 raters!'))}
    
    w <- warning_handler(do.call(paste0(choice, '.weights', collapse = ''), args = list(min(data):max(data)))[1,])
    
    bangdi_data <<- table(factor(data[,1], min(data):max(data)), factor(data[,2], min(data):max(data)))
    
    vals_bangdiwala <- list('vals' = warning_handler(vcd::agreementplot(bangdi_data,
                                                                        weights = if(!all(is.nan(w))) {w} else {c(1, 0)},
                                                                        return_grob = T)),
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







#source('style.R')

other_randIndex <- tabItem(tabName = 'other_randIndex',
                     fluidRow(
                       column(
                         width = 10,
                         offset = 1,
                         style = 'padding-left: 0px; padding-right: -5px;',
                         box(
                           id = 'randIndexDocum',
                           width = NULL,
                           style = measure_title_style,
                           h3("The Adjusted Rand Index")
                         ),
                         hidden(
                           div(id = 'randIndexDocumBox',
                               fluidRow(class = 'documRow',
                                        column(width = 12,
                                               offset = 0,
                                               box(title = randIntex_docum_text,
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
                             p(file_upload_text),
                             style = 'text-align: center;'
                           )
                         ),
                         fluidRow(
                           box(
                             width = NULL,
                             p(file_struct_text),
                             look_down,
                             style = 'text-align: center;'
                           )
                         )
                       )
                       ,
                       column(width = 5,
                              box(
                                width = NULL,
                                fileInput(inputId = 'randIndexInput',
                                          label = 'Browse for .csv files'),
                                actionButton(
                                  inputId = 'randIndexRun',
                                  label = 'calculate'
                                )
                              ),
                              style = "text-align: center;"
                              )
                     ),
                     fluidRow(class = 'tabStyle',
                              column(
                                width = 5,
                                offset = 1,
                                style = 'padding: 0px;',
                                uiOutput('ui_randIndex')
                              ),
                              
                              column(width = 5,
                                     shinyWidgets::dropMenu(
                                       div(id = 'randIndexDrop',
                                           fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                                                    column(
                                                      width = 12,
                                                      valueBoxOutput(outputId = 'randIndex', width = NULL)
                                                    )
                                           )
                                       ),
                                       HTML(kableExtra::kable(t(randIndex_output_description)) %>% 
                                              kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri')),
                                       trigger = 'mouseenter',
                                       theme = 'translucent',
                                       placement = 'left-start')
                              )
                     ))







randIndexOut <- function(input, output, data) {
  
  tryCatch({
  
    vals_randIndex <- list('vals' = warning_handler(randMain(data)),
                           'warn' = msg)
    
    d_randIndex <- warning_handler(t(data.frame(t(vals_randIndex$vals))))
    
    l_randIndex <<- warning_handler(as.data.frame(d_randIndex))
    
    output$randIndex <- renderValueBox({
      valueBox(
        subtitle = p(HTML(paste0(
          kableExtra::kable(d_randIndex, format = 'html') %>% 
            kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri'),
          
        
        if (!is.null(vals_randIndex$warn)) {
          circleButton(inputId = 'warningButton',
                       icon = icon("exclamation"),
                       size = 's')
        }
        )),
        div(
          downloadButton(outputId = 'randIndexFullDown',
                         label = 'Full Results'),
          style = 'text-align: center;'
        )),
        value = ''
      )
    })
  
  }, error = function(e) {
    invalid_data(output, 'randIndex')
    print(paste('randIndex error happend: ', e))
  }, warning = function(w) {
    invalid_data(output, 'randIndex')
    print(paste('randIndex warning happened: ', w))
  })
  
}

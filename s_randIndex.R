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
                           style = 'text-align:center; padding: 30px;',
                           h3("The Adjusted Rand Index")
                         ),
                         hidden(
                           div(id = 'randIndexDocumBox',
                               fluidRow(class = 'documRow',
                                        column(width = 12,
                                               offset = 0,
                                               box(title = randIntex_docum_text,
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
                              
                              column(
                                width = 5,
                                fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                                         column(
                                           width = 12,
                                           shinyBS::popify(valueBoxOutput(outputId = 'randIndex', width = NULL), 
                                                           title = 'What means what',
                                                           content = paste0('<li>', names(randIndex_output_description),
                                                                            ' = ',
                                                                            as.character(randIndex_output_description), '</li>',
                                                                            br()),
                                                           placement = 'left'
                                           )
                                         ))
                              )
                     ))







randIndexOut <- function(input, output, data) {
  
  tryCatch({
  
    vals_randIndex <- list('vals' = warning_handler(randMain(data)),
                           'warn' = msg)
    
    print(vals_randIndex$vals)
    
    d_randIndex <- warning_handler(t(data.frame(vals_randIndex$vals)))
    
    l_randIndex <<- warning_handler(as.data.frame(d_randIndex))
    
    output$randIndex <- renderValueBox({
      valueBox(
        subtitle = p(HTML(paste0(
          kableExtra::kable(round(d_randIndex, 3), format = 'html') %>% 
            kableExtra::kable_styling('basic'),
          
        
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


krippendorf <- tabItem(tabName = 'krippendorf',
                 useShinyjs(),
                 fluidRow(
                   column(
                     width = 10,
                     offset = 1,
                     style = 'padding-left: 0px; padding-right: -5px;',
                     box(
                       width = NULL,
                       style = 'text-align:center; padding: 30px;',
                       h3(id = 'krippDocum',
                          "Krippendorf's Alpha")
                     ),
                     hidden(
                       div(id = 'krippDocumBox',
                           fluidRow(class = 'documRow',
                                    column(width = 12,
                                           offset = 0,
                                           box(title = kripp_docum_text,
                                               width = NULL)
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
                         height = '131px',
                         p(file_upload_text),
                         style = 'text-align: center;'
                       )
                     ),
                     fluidRow(
                       box(
                         width = NULL,
                         height = '131px',
                         p(file_struct_text),
                         look_down,
                         style = 'text-align: center;'
                       )
                     )
                   ),
                   column(
                     width = 5,
                     box(width = NULL,
                         height = '282px',
                         #custom js to change upload complete text
                         fileInput(inputId = 'krippInput',
                                   label = 'Browse for .csv files',
                                   accept = ".csv"),
                         style = 'text-align: center;',
                         div(class = 'checkbox_button_css',
                             h5('Data Scale Level'),
                             br(),
                             checkboxGroupButtons(inputId = 'krippChoice',
                                                  choices = c('nominal',
                                                              'ordinal',
                                                              'interval',
                                                              'ratio'),
                                                              status = 'krippbtn1',
                                                              justified = T,
                                                              individual = F,
                                                              size = 'n',
                                                              direction = 'horizontal'))
                         ,
                         actionButton(inputId = 'krippRun',
                                      label = 'calculate')
                     )
                   )
                 ),
                 fluidRow(class = 'tabStyle',
                          column(
                            width = 5, 
                            offset = 1,
                            style = 'padding: 0px;',
                            uiOutput('ui_kripp')
                          ),
                          column(
                            width = 5,
                            fluidRow(class = 'style_valuebox_KRIPP_cyan',
                                     column(
                                       width = 12,
                                         valueBoxOutput(
                                           outputId = 'kripp1',
                                           width = NULL)
                                     )
                            )
                          )
                 )
)

krippMainOut <- function(input, output, data) {
  
  tryCatch({
    
    test <- krippMain(input, output, data)
    alpha <- test$value
    l_kripp <<- lapply(test, as.data.frame)
    
  }, error = function(e) {
    print(e)
  }, warning = function(w) {
    print(w)
  })
  
  output$kripp1 <- renderValueBox({
    tryCatch({
      
      valueBox(
        subtitle = p(HTML(paste0(
          'Alpha: ', round(as.numeric(alpha), 3), br(),br(),
          downloadButton(outputId = 'krippFullDown',
                         label = 'Full Results')
        )),
        style = 'text-align: center; font-size: 20px;'),
        value = ''
      )
      
      
    }, error = function(e) {
      print(e)
    }, warning = function(w) {
      print(w)
    })
    
  })
}
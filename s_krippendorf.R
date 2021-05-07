

krippendorf <- tabItem(tabName = 'krippendorf',
                 useShinyjs(),
                 fluidRow(
                   column(
                     width = 10,
                     offset = 1,
                     style = 'padding-left: 0px; padding-right: -5px;',
                     box(
                       id = 'krippDocum',
                       width = NULL,
                       style = 'text-align:center; padding: 0;',
                       h3("Krippendorf's Alpha")
                     ),
                     hidden(
                       div(id = 'krippDocumBox',
                           fluidRow(class = 'documRow',
                                    column(width = 12,
                                           offset = 0,
                                           box(title = kripp_docum_text,
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
                                       shinyBS::popify(valueBoxOutput(outputId = 'kripp1', width = NULL), 
                                                       title = 'What means what',
                                                       content = paste0('<li>', names(kripp1_output_description),
                                                                        ' = ',
                                                                        as.character(kripp1_output_description), '</li>',
                                                                        br()),
                                                       placement = 'left'
                                       )
                                     )
                            )
                          )
                 )
)

krippMainOut <- function(input, output, data) {
  
  tryCatch({
    
    test <- warning_handler(krippMain(input, output, data))
    alpha <- test$alpha.hat
    
    test$call <- NULL
    test$dist <- NULL
    
    l_kripp <<- lapply(test, as.data.frame)
    
    d_kripp <- data.frame('est.kripp' = alpha)
    
    ci <- confint(test, 0.95)
    
    d_kripp$lb <- ci[1]
    d_kripp$ub <- ci[2]
    
    d_kripp <- t(d_kripp)
    
  }, error = function(e) {
    print(e)
  }, warning = function(w) {
    print(w)
  })
  
  output$kripp1 <- renderValueBox({
    tryCatch({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_kripp, format = 'html') %>% 
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
          downloadButton(outputId = 'krippFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
      
      
    }, error = function(e) {
      print(e)
    }, warning = function(w) {
      print(w)
    })
    
  })
}

bfun_kripp <- function(d,i) {
  dat <- t(d[i,])
  return(irr::kripp.alpha(dat, choice))
}

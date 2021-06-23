other_kripp <- tabItem(
  tabName = 'other_kripp',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    id = 'krippDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("kripp & Prediger's Kappa")
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
        fileInput(inputId = 'krippInput',
                  label = 'Browse for .csv files'),
        h5('Choose weighting method'),
        div(class = 'selectInputStyle',
            selectInput(inputId = 'kripp_weight',
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
          inputId = 'krippRun',
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
      uiOutput('ui_kripp')
    ),
    
    column(width = 5,
           shinyWidgets::dropMenu(
             div(id = 'krippDrop',
                 fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                          column(
                            width = 12,
                            valueBoxOutput(outputId = 'kripp1', width = NULL)
                          )
                 )
             ),
             HTML(kableExtra::kable(t(kripp_output_description)) %>% 
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
krippOut <- function(input, output, data) {
  
  tryCatch({
    
    choice <- input$kripp_weight
    
    vals_kripp <- list('vals' = warning_handler(irrCAC::krippen.alpha.raw(data,
                                                                       if(!is.null(choice)) {weights = choice} 
                                                                       else { 
                                                                         weights = 'unweighted'
                                                                       }
    )$est
    ),
    'warn' = msg)
    
    l_kripp <<- lapply(vals_kripp$vals, as.data.frame)
    
    class(vals_kripp$vals) <- 'list'
    
    d_kripp <- t(as.data.frame(vals_kripp$vals))
    
    output$kripp1 <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_kripp, format = 'html') %>% 
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
          downloadButton(outputId = 'krippFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'kripp1')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'kripp1')
    print(w)
  })
}

# other_kripp <- tabItem(tabName = 'other_kripp',
#                  useShinyjs(),
#                  fluidRow(
#                    column(
#                      width = 10,
#                      offset = 1,
#                      style = 'padding-left: 0px; padding-right: -5px;',
#                      box(
#                        id = 'krippDocum',
#                        width = NULL,
#                        style = 'text-align:center; padding: 0;',
#                        h3("Krippendorf's Alpha")
#                      ),
#                      hidden(
#                        div(id = 'krippDocumBox',
#                            fluidRow(class = 'documRow',
#                                     column(width = 12,
#                                            offset = 0,
#                                            box(title = kripp_docum_text,
#                                                width = NULL)
#                                     )
#                            )
#                        )
#                      )
#                    )
#                  ),
#                  fluidRow(
#                    column(
#                      width = 5,
#                      offset = 1,
#                      fluidRow(
#                        box(
#                          width = NULL,
#                          height = '131px',
#                          p(file_upload_text),
#                          style = 'text-align: center;'
#                        )
#                      ),
#                      fluidRow(
#                        box(
#                          width = NULL,
#                          height = '131px',
#                          p(file_struct_text),
#                          look_down,
#                          style = 'text-align: center;'
#                        )
#                      )
#                    ),
#                    column(
#                      width = 5,
#                      box(width = NULL,
#                          height = '282px',
#                          #custom js to change upload complete text
#                          fileInput(inputId = 'krippInput',
#                                    label = 'Browse for .csv files',
#                                    accept = ".csv"),
#                          style = 'text-align: center;',
#                          div(class = 'checkbox_button_css',
#                              h5('Data Scale Level'),
#                              br(),
#                              checkboxGroupButtons(inputId = 'krippChoice',
#                                                   choices = c('nominal',
#                                                               'ordinal',
#                                                               'interval',
#                                                               'ratio'),
#                                                               status = 'krippbtn1',
#                                                               justified = T,
#                                                               individual = F,
#                                                               size = 'n',
#                                                               direction = 'horizontal'))
#                          ,
#                          actionButton(inputId = 'krippRun',
#                                       label = 'calculate')
#                      )
#                    )
#                  ),
#                  fluidRow(class = 'tabStyle',
#                           column(
#                             width = 5, 
#                             offset = 1,
#                             style = 'padding: 0px;',
#                             uiOutput('ui_kripp')
#                           ),
#                           column(width = 5,
#                                  shinyWidgets::dropMenu(
#                                    div(id = 'krippDrop',
#                                        fluidRow(class = 'style_valuebox_OUTPUT_cyan',
#                                                 column(
#                                                   width = 12,
#                                                   valueBoxOutput(outputId = 'kripp1', width = NULL)
#                                                 )
#                                        )
#                                    ),
#                                    HTML(kableExtra::kable(t(kripp1_output_description)) %>% 
#                                           kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri')),
#                                    trigger = 'mouseenter',
#                                    theme = 'translucent',
#                                    placement = 'left-start')
#                           )
#                  )
# )
# 
# krippMainOut <- function(input, output, data) {
#   
#   tryCatch({
#     
#     test <- warning_handler(krippMain(input, output, data))
#     alpha <- test$alpha.hat
#     
#     test$call <- NULL
#     test$dist <- NULL
#     
#     l_kripp <<- lapply(test, as.data.frame)
#     
#     d_kripp <- data.frame('est.kripp' = alpha)
#     
#     ci <- confint(test, 0.95)
#     
#     d_kripp$lb <- ci[1]
#     d_kripp$ub <- ci[2]
#     
#     d_kripp <- t(d_kripp)
#     
#   }, error = function(e) {
#     print(e)
#   }, warning = function(w) {
#     print(w)
#   })
#   
#   output$kripp1 <- renderValueBox({
#     tryCatch({
#       
#       valueBox(
#         subtitle = p(HTML(
#           kableExtra::kable(d_kripp, format = 'html') %>% 
#             kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri'),
#           
#         ),
#         div(
#           if(!is.null(msg)) {
#             p(HTML(paste0(
#               circleButton(inputId = 'warningButton',
#                            icon = icon("exclamation"),
#                            size = 's'),
#               br()
#             )))
#           },
#           style = centerText
#         ),
#         div(
#           downloadButton(outputId = 'krippFullDown',
#                          label = 'Full Results'),
#           style = centerText
#         )),
#         value = ''
#       )
#       
#       
#     }, error = function(e) {
#       print(e)
#     }, warning = function(w) {
#       print(w)
#     })
#     
#   })
# }
# 
# bfun_kripp <- function(d,i) {
#   dat <- t(d[i,])
#   return(irr::kripp.alpha(dat, choice))
# }

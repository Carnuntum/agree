
other_occc <- tabItem(
  tabName = 'other_occc',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    id = 'occcDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("Concordance Correlation Coefficient")
                  ),
                  hidden(
                    div(id = 'occcDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = occc_docum_text,
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
        fileInput(inputId = 'occcInput',
                  label = 'Browse for .csv files'),
        actionButton(
          inputId = 'occcRun',
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
      uiOutput('ui_occc')
    ),
    
    column(width = 5,
           fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                    column(
                      width = 12,
                      shinyBS::popify(valueBoxOutput(outputId = 'occc', width = NULL), 
                                      title = 'What means what',
                                      content = paste0('<li>', names(occc_output_description),
                                                       ' = ',
                                                       as.character(occc_output_description), '</li>',
                                                       br()),
                                      placement = 'left'
                      )
                    )
           )
    )
  ),
  fluidRow(
    class = 'tabStyle',
    column(
      width = 8,
      offset = 2,
      style = 'padding: 0;',
      shinyjs::hidden(
        div(id = 'div_plot_occc',
            box(
              title = 'Bland Altman Plot',
              width = NULL,
              plotOutput('plot_occc'),
              downloadButton('down_plot_occc', style = 'margin: 20px;')
            ),
            style = centerText
        )
      )
    )
  ),
  fluidRow(column(width = 12,
                  style = 'height: 250px;'))
)




#-------------------------------------------------------------------------------
#'*--------------- CALCULATED OUTPUT FOR PERCENT AGREEMENT --------------------*
#-------------------------------------------------------------------------------
#chi output for corrected test
occcOut <- function(input, output, data, scope = F) {
  
  tryCatch({
    
    
    
    vals_occc <- list('vals' = warning_handler(epiR::epi.occc(data, pairs = T, na.rm = T)),
                      'warn' = msg)
    
    bfun <- function(d, i) {
      dat <- d[i,]
      return(epiR::epi.occc(dat)$occc)
    }
    
    ci <- warning_handler(makeCi(data, bfun))
    
    vals_scale_location <- as.data.frame(vals_occc$vals$pairs) %>% dplyr::select(-ksi, -ccc)
    
    l_occc <<- lapply(vals_occc$vals, as.data.frame)
    
    l_occc <<- append(l_occc, vals_scale_location)
    
    d_occc <- as.data.frame(vals_occc$vals[1:3])
    d_occc$lb <- ci[1]
    d_occc$ub <- ci[2]
    
    if(dim(data)[2] == 2) {
      shinyjs::show('div_plot_occc', anim = T)
      output$plot_occc <- renderPlot({
        blaltm <<- BlandAltmanLeh::bland.altman.plot(group1 = data[,1], group2 = data[,2],
                                                     graph.sys = 'ggplot2')
        blaltm
      })
    }
    
    if(scope == F) {
      set_tabset(output, out_id = 'ui_occc', id = 'id_occc',
                 tableId = 'tab_occc', downId = 'down_occc',
                 btnInputId = 'test_occc', data = data)
    } else {
      set_tabset(output, out_id = 'ui_occc', id = 'id_occc', 'Uploads', 'Your Data',
                 tableId = 'tab_occc', downId = 'down_occc',
                 btnInputId = 'test_occc', data = data)
    }
    
    output$occc <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(paste0(
          kableExtra::kable(t(d_occc), format = 'html') %>% 
            kableExtra::kable_styling('basic'),
          
          kableExtra::kable(vals_scale_location, format = 'html') %>% 
            kableExtra::kable_styling('basic')
        )),
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
          downloadButton(outputId = 'occcFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'occc')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'occc')
    print(w)
  })
}

# occc <- tabItem(
#   tabName = 'occc',
#   useShinyjs(),
#   fluidRow(
#     column(
#       width = 10,
#       offset = 1,
#       style = 'padding-left: 0px; padding-right: -5px;',
#       box(
#         width = NULL,
#         style = 'text-align:center; padding: 30px;',
#         h3(id = 'occcDocum',
#            "Concordance Correlation Coefficient")
#       )
#     )
#   ),
#   fluidRow(
#     column(
#       width = 5,
#       offset = 1,
#       fluidRow(
#         box(
#           width = NULL,
#           height = '131px',
#           p(file_upload_text),
#           style = 'text-align: center;'
#         )
#       ),
#       fluidRow(
#         box(
#           width = NULL,
#           height = '131px',
#           p(file_struct_text),
#           look_down,
#           style = 'text-align: center;'
#         )
#       )
#     ),
#     column(
#       width = 5,
#       box(
#         width = NULL,
#         #custom js to change upload complete text
#         fileInput(
#           inputId = 'occcInput',
#           label = 'Browse for .csv files',
#           accept = ".csv"
#         ),
#         style = 'text-align: center;',
#         div(
#           class = 'checkbox_button_css',
#           h5('CI Options'),
#           checkboxGroupButtons(
#             inputId = 'occcChoice',
#             choices = c('z-transform', 'asymptotic'),
#             status = 'occcbtn1',
#             justified = T,
#             individual = F,
#             size = 'n',
#             direction = 'horizontal'
#           )
#         ),
#         actionButton(inputId = 'occcRun',
#                      label = 'calculate')
#       )
#     )
#   ),
#   fluidRow(
#     class = 'tabStyle',
#     column(
#       width = 5,
#       offset = 1,
#       style = 'padding: 0px;',
#       uiOutput('ui_occc'),
#     ),
#     column(width = 5,
#            fluidRow(
#              class = 'style_valuebox_OUTPUT_cyan',
#              column(width = 12,
#                     valueBoxOutput(outputId = 'occc',
#                                    width = NULL))
#            ))
#   ),
#   fluidRow(
#     class = 'tabStyle',
#     column(
#       width = 8,
#       offset = 2,
#       style = 'padding: 0;',
#       shinyjs::hidden(
#         div(id = 'div_plot_occc',
#             box(
#               title = 'Bland Altman Plot',
#               width = NULL,
#               plotOutput('plot_occc'),
#               downloadButton('down_plot_occc', style = 'margin: 20px;')
#             ),
#             style = 'text-align: center;'
#         )
#       )
#     )
#   ),
#   fluidRow(column(width = 12,
#                   style = 'height: 250px;'))
# )
# 
# occcMainOut <- function(input, output, data) {
#   
#   
#   
#   
#   tryCatch({
#     
#     if(dim(data)[2] == 2) {
#       test <- occcMain(input, output, data)
#       
#       l_occc <<- lapply(test, as.data.frame)
#       
#       d_occc <- t(data.frame(
#         'Rho' = round(as.numeric(test$rho.c[1]), 3),
#         'Lower CI' = round(as.numeric(test$rho.c[2]), 3), 
#         'Upper CI' = round(as.numeric(test$rho.c[3]), 3), 
#         'Scale Shift' = round(as.numeric(test$s.shift), 3), 
#         'Location Shift' = round(as.numeric(test$l.shift), 3), 
#         'Bias Correction' = round(as.numeric(test$C.b), 3)
#       ))
#       
#       
#       shinyjs::show('div_plot_occc', anim = T)
#       output$plot_occc <- renderPlot({
#         blaltm <<- BlandAltmanLeh::bland.altman.plot(group1 = data[,1], group2 = data[,2],
#                                                      graph.sys = 'ggplot2')
#         blaltm
#       })
#     } else {
#       
#     }
#   
#   output$ccc1 <- renderValueBox({
#     tryCatch({
#       valueBox(
#         value = p(HTML(
#           kableExtra::kable(d_ccc, format = 'html') %>% 
#             kableExtra::kable_styling('basic'),
#           
#         ),
#         div(
#           downloadButton(outputId = 'cccFullDown',
#                          label = 'Full Results'),
#           style = 'text-align: center;'
#         )),
#         subtitle = ''
#       )
#     }, error = function(e) {
#       print('ccc_out_e')
#       print(e)
#     }, warning = function(w) {
#       print('ccc_out_w')
#       print(w)
#     })
#   })
#   
#   }, error = function(e) {
#     print('ccc_test_e')
#     print(e)
#   }, warning = function(w) {
#     print('ccc_test_w')
#     print(w)
#   })
# }
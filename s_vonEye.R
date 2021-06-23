
kappa_eye <- tabItem(
  tabName = 'kappa_eye',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px;',
                  box(
                    id = 'eyeDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("Von Eye's Kappa")
                  ),
                  hidden(
                    div(id = 'eyeDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = eye_docum_text,
                                            width = NULL,
                                            style = 'text-align:center; padding: 0;')
                                 )
                        )
                    )
                  )
  )),
  
  fluidRow(
    
    column(
      width = 5,
      offset = 1,
      fluidRow(box(
        width = NULL,
        height = '105px',
        p(file_upload_text),
        style = centerText
      )),
      fluidRow(
        box(
          width = NULL,
          height = '105px',
          p(file_struct_text),
          look_down,
          style = centerText
        )
      ),
      fluidRow(column(
        class = 'tabStyle',
        width = 12,
        offset = 0,
        style = 'padding: 0px;',
        uiOutput('ui_eye')
      ))),
    column(
      width = 5,
      box(
        width = NULL,
        fileInput(inputId = 'eyeInput',
                  label = 'Browse for .csv files'),
        actionButton(inputId = 'eyeRun',
                     label = 'calculate'),
        style = centerText
      ),
      fluidRow(column(width = 12,
             shinyWidgets::dropMenu(
               div(id = 'eyeDrop',
                   fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                            column(
                              width = 12,
                              valueBoxOutput(outputId = 'eye', width = NULL)
                            )
                   )
               ),
               HTML(kableExtra::kable(t(eye_output_description)) %>% 
                      kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri')),
               trigger = 'mouseenter',
               theme = 'translucent',
               placement = 'left-start')
      ))
    )
  )
)




#-------------------------------------------------------------------------------
#'*--------------- CALCULATED OUTPUT FOR PERCENT AGREEMENT --------------------*
#-------------------------------------------------------------------------------
#chi output for corrected test
eyeOut <- function(input, output, data, scope = F) {
  
  tryCatch({
    
    data <- na.omit(data)
    
    vals_eye <- list('vals' = warning_handler(Ks(data)),
                     'warn' = msg)
    
    l_eye <<- lapply(vals_eye$vals, as.data.frame)
    
    d_eye <- t(as.data.frame(vals_eye$vals))
    
    
    output$eye <- renderValueBox({
      
      if(scope == F) {
        set_tabset(output, 'ui_eye', id = 'id_eye', tableId = 'tab_eye', btnInputId = 'test_eye',
                   downId = 'down_eye', data = data)
      } else {
        set_tabset(output, 'ui_eye', id = 'id_eye', 'Uploads', 'Your Data', tableId = 'tab_eye',
                   btnInputId = 'test_eye', downId = 'down_eye', data = data)
      }
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_eye, format = 'html') %>% 
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
          downloadButton(outputId = 'eyeFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'eye')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'eye')
    print(w)
  })
}









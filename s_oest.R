
kappa_oest <- tabItem(
  tabName = 'kappa_oest',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px;',
                  box(
                    id = 'oestDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("van Oest's Kappa")
                  ),
                  hidden(
                    div(id = 'oestDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = oest_docum_text,
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
        uiOutput('ui_oest')
      ))),
    column(
      width = 5,
      box(
        width = NULL,
        fileInput(inputId = 'oestInput',
                  label = 'Browse for .csv files'),
        actionButton(inputId = 'oestRun',
                     label = 'calculate'),
        style = centerText
      ),
      fluidRow(column(width = 12,
                      shinyWidgets::dropMenu(
                        div(id = 'oestDrop',
                            fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                                     column(
                                       width = 12,
                                       valueBoxOutput(outputId = 'oest', width = NULL)
                                     )
                            )
                        ),
                        HTML(kableExtra::kable(t(oest_output_description)) %>% 
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
oestOut <- function(input, output, data, scope = F) {
  
  tryCatch({
    
    data <- na.omit(data)
    
    vals_oest <- list('vals' = warning_handler(list('est' = Ir(data))),
                     'warn' = msg)
    
    ci <- warning_handler(makeCi(data, bfun_oest, 2000))
    
    vals_oest$vals$lb <- ci[1]
    vals_oest$vals$ub <- ci[2]
    
    l_oest <<- lapply(vals_oest$vals, as.data.frame)
    
    d_oest <- t(as.data.frame(vals_oest$vals))
    
    
    if(scope == F) {
      set_tabset(output, 'ui_oest', id = 'id_oest', tableId = 'tab_oest', btnInputId = 'test_oest',
                 downId = 'down_oest', data = data)
    } else {
      set_tabset(output, 'ui_oest', id = 'id_oest', 'Uploads', 'Your Data', tableId = 'tab_oest',
                 btnInputId = 'test_oest', downId = 'down_oest', data = data)
    }
    
    
    output$oest <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_oest, format = 'html') %>% 
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
          downloadButton(outputId = 'oestFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'oest')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'oest')
    print(w)
  })
}

bfun_oest <- function(d,i) {
  dat <- d[i,]
  return(Ir(dat))
}







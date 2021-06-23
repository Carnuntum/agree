
other_infoAgree <- tabItem(
  tabName = 'other_infoAgree',
    fluidRow(column(width = 10, 
                    offset = 1,
                    style = 'padding-left: 0px;',
                    box(
                      id = 'infoAgreeDocum',
                      width = NULL,
                      style = measure_title_style,
                      h3("Information Agreement")
                    ),
                    hidden(
                      div(id = 'infoAgreeDocumBox',
                          fluidRow(class = 'documRow',
                                   column(width = 12,
                                          offset = 0,
                                          box(title = infoAgree_docum_text,
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
          uiOutput('ui_infoAgree')
        ))),
      column(
        width = 5,
        box(
          width = NULL,
          fileInput(inputId = 'infoAgreeInput',
                    label = 'Browse for .csv files'),
          actionButton(inputId = 'infoAgreeRun',
                       label = 'calculate'),
          style = centerText
        ),
        fluidRow(column(width = 12,
                        shinyWidgets::dropMenu(
                          div(id = 'infoAgreeDrop',
                              fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                                       column(
                                         width = 12,
                                         valueBoxOutput(outputId = 'infoAgree', width = NULL)
                                       )
                              )
                          ),
                          HTML(kableExtra::kable(t(infoAgree_output_description)) %>% 
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
infoAgreeOut <- function(input, output, data, scope = F) {
  
  tryCatch({
    
    data <- warning_handler(na.omit(data))
    
    vals_infoAgree <- list('vals' = warning_handler(list('ia' = ia_c(data))),
                           'warn' = msg)
    
    
    l_infoAgree <<- lapply(vals_infoAgree$vals, as.data.frame)
    
    ci <- warning_handler(makeCi(data, bfun_ia))
    
    vals_infoAgree$vals$lb <- ci[1]
    vals_infoAgree$vals$ub <- ci[2]
    
    d_infoAgree <- t(as.data.frame(vals_infoAgree$vals))
    
    
    if(scope == F) {
      set_tabset(output, 'ui_infoAgree', id = 'id_infoAgree', tableId = 'tab_infoAgree', btnInputId = 'test_infoAgree',
                 downId = 'down_infoAgree', data = data)
    } else {
      set_tabset(output, 'ui_infoAgree', id = 'id_infoAgree', 'Uploads', 'Your Data', tableId = 'tab_infoAgree',
                 btnInputId = 'test_infoAgree', downId = 'down_infoAgree', data = data)
    }
    
    output$infoAgree <- renderValueBox({
      
      
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_infoAgree, format = 'html') %>% 
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
          downloadButton(outputId = 'infoAgreeFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'infoAgree')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'infoAgree')
    print(w)
  })
}



bfun_ia <- function(d,i) {
  dat <- d[i,]
  return(ia_c(dat))
}








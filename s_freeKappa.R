
kappa_free <- tabItem(
  tabName = 'kappa_free',
    fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px;',
                  box(
                    id = 'freeKappaDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("Carpentier's Kappa")
                  ),
                  hidden(
                    div(id = 'freeKappaDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = freeKappa_docum_text,
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
        uiOutput('ui_freeKappa')
      ))),
    column(
      width = 5,
      box(
        width = NULL,
        fileInput(inputId = 'freeKappaInput',
                  label = 'Browse for .csv files'),
        actionButton(inputId = 'freeKappaRun',
                     label = 'calculate'),
        style = centerText
      ),
      fluidRow(column(width = 12,
                      shinyWidgets::dropMenu(
                        div(id = 'freeKappaDrop',
                            fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                                     column(
                                       width = 12,
                                       valueBoxOutput(outputId = 'freeKappa', width = NULL)
                                     )
                            )
                        ),
                        HTML(kableExtra::kable(t(freeKappa_output_description)) %>% 
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
freeKappaOut <- function(input, output, data, scope) {
  
  tryCatch({
    
    warning_handler(
      if(ncol(data) > 2 || length(unique(unlist(as.list(data)))) > 2) {
        stop('more than 2 raters or categories. did you upload valid data?')
      }
    )
    
    data <- na.omit(data)
    
    vals_freeKappa <- list('vals' = warning_handler(freeKappa(data)),
                      'warn' = msg)
    
    l_freeKappa <<- lapply(vals_freeKappa$vals, as.data.frame)
    
    ci <- warning_handler(makeCi(data, bfun_freeKappa))
    
    vals_freeKappa$vals$lb <- ci[1]
    vals_freeKappa$vals$ub <- ci[2]
    
    d_freeKappa <- t(as.data.frame(vals_freeKappa$vals))
    
    
    if(scope == F) {
      set_tabset(output, 'ui_freeKappa', id = 'id_freeKappa', tableId = 'tab_freeKappa', btnInputId = 'test_freeKappa',
                 downId = 'down_freeKappa', data = data)
    } else {
      set_tabset(output, 'ui_freeKappa', id = 'id_freeKappa', 'Uploads', 'Your Data', tableId = 'tab_freeKappa',
                 btnInputId = 'test_freeKappa', downId = 'down_freeKappa', data = data)
    }
    
    output$freeKappa <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_freeKappa, format = 'html') %>% 
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
          downloadButton(outputId = 'freeKappaFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'freeKappa')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'freeKappa')
    print(w)
  })
}









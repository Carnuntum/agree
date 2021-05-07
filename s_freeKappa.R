
kappa_free <- tabItem(
  tabName = 'kappa_free',
  fluidRow(
    fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    id = 'freeKappaDocum',
                    width = 12,
                    style = measure_title_style,
                    h3("Carpentier's Kappa")
                  )
  )),
  hidden(
    div(id = 'freeKappaDocumBox',
        fluidRow(class = 'documRow',
                 column(width = 10,
                        offset = 1,
                        style = 'padding-left: 0px; padding-right: -5px;',
                        box(title = freeKappa_docum_text,
                            width = 12,
                            style = 'text-align:center; padding: 0;')
                 )
        )
    )
  ),
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
    column(
      width = 12,
      fluidRow(
        class = 'style_valuebox_OUTPUT_cyan',
        
        width = 12,
        shinyBS::popify(valueBoxOutput(outputId = 'freeKappa', width = NULL), 
                        title = 'What means what',
                        content = paste0('<li>', names(freeKappa_output_description),
                                         ' = ',
                                         as.character(freeKappa_output_description), '</li>',
                                         br()),
                        placement = 'left'
        )
      )
    )
  ))
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









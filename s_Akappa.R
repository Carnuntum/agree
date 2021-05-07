
kappa_akappa <- tabItem(
  tabName = 'kappa_akappa',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    id = 'akappaDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("A Kappa")
                  ),
                  hidden(
                    div(id = 'akappaDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = akappa_docum_text,
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
        fileInput(inputId = 'akappaInput',
                  label = 'Browse for .csv files'),
        actionButton(
          inputId = 'akappaRun',
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
      uiOutput('ui_akappa')
    ),
    
    column(width = 5,
           fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                    column(
                      width = 12,
                      shinyBS::popify(valueBoxOutput(outputId = 'akappa', width = NULL), 
                                      title = 'What means what',
                                      content = paste0('<li>', names(akappa_output_description),
                                                       ' = ',
                                                       as.character(akappa_output_description), '</li>',
                                                       br()),
                                      placement = 'left'
                      )
                    )
           )
    )
  )
)




#-------------------------------------------------------------------------------
#'*--------------------- CALCULATED OUTPUT FOR A KAPPA ------------------------*
#-------------------------------------------------------------------------------
#chi output for corrected test
akappaOut <- function(input, output, data) {
  
  tryCatch({
    data <- na.omit(data)
    
    vals_akappa <- list('vals' = warning_handler(list('A-kappa' = aKappa(data))),
                        'warn' = msg)
    
    ci <- warning_handler(makeCi(data, bfun_akappa, n = 500))

    vals_akappa$vals$lb <- ci[1]
    vals_akappa$vals$ub <- ci[2]
    
    l_akappa <<- lapply(vals_akappa$vals, as.data.frame)
    
    d_akappa <- t(as.data.frame(vals_akappa$vals))
    
    output$akappa <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_akappa, format = 'html') %>% 
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
          downloadButton(outputId = 'akappaFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'akappa')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'akappa')
    print(w)
  })
}

bfun_akappa <- function(d, i) {
  dat <- d[i,]
  return(aKappa(dat))
}

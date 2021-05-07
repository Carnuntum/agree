
other_entropy <- tabItem(
  tabName = 'other_entropy',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    id = 'entropyDocum',
                    width = 12,
                    style = measure_title_style,
                    h3("The Double Entropy Index")
                  )
  )),
  
  fluidRow(
    hidden(
      div(id = 'entropyDocumBox',
          fluidRow(class = 'documRow',
                   column(width = 10,
                          offset = 1,
                          style = 'padding-left: 0px; padding-right: -5px;',
                          box(title = entropy_docum_text,
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
        uiOutput('ui_entropy')
      ))),
    column(
      width = 5,
      box(
        width = NULL,
        fileInput(inputId = 'entropyInput',
                  label = 'Browse for .csv files'),
        actionButton(inputId = 'entropyRun',
                     label = 'calculate'),
        style = centerText
      ),
      column(
        width = 12,
        fluidRow(
          class = 'style_valuebox_OUTPUT_cyan',
          
          width = 12,
          shinycssloaders::withSpinner(
            type = getOption("spinner.type", default = 7),
            color = getOption("spinner.color", default = "#3c8dbc"),
            proxy.height = '200px',
            
            ui_element = shinyBS::popify(valueBoxOutput(outputId = 'entropy', width = NULL), 
                                         title = 'What means what',
                                         content = paste0('<li>', names(entropy_output_description),
                                                          ' = ',
                                                          as.character(entropy_output_description), '</li>',
                                                          br()),
                                         placement = 'left'
            ),
            
            id = 'entropySpinner'
          )
        )
      )
    )
  )
)




#-------------------------------------------------------------------------------
#'*--------------- CALCULATED OUTPUT FOR PERCENT AGREEMENT --------------------*
#-------------------------------------------------------------------------------
#chi output for corrected test
entropyOut <- function(input, output, data, scope = F) {
  
  tryCatch({
    
    output$entropy <- renderValueBox({
      
      tryCatch({
        isolate({
          
          data <- na.omit(data)
          
          if(ncol(data) == 1) {
            vals <- warning_handler(doubleEntropy(data))
          } else {
            vals <- warning_handler(mean(apply(data, 2, doubleEntropy)))
          }
          
          vals_entropy <- list('vals' = list('e.index' = vals),
                               'warn' = msg)
          
          l_entropy <<- lapply(vals_entropy$vals, as.data.frame)
          
          ci <- warning_handler(makeCi(data, bfun_entropy))
          
          vals_entropy$vals$lb <- ci[1]
          vals_entropy$vals$ub <- ci[2]
          
          d_entropy <- t(as.data.frame(vals_entropy$vals))
          
        })
      }, error = function(e) {
        invalid_data(output, 'entropy')
        print(e)
      }, warning = function(w) {
        invalid_data(output, 'entropy')
        print(w)
      })
      
      if(scope == F) {
        set_tabset(output, 'ui_entropy', id = 'id_entropy', tableId = 'tab_entropy', btnInputId = 'test_entropy',
                   downId = 'down_entropy', data = data)
      } else {
        set_tabset(output, 'ui_entropy', id = 'id_entropy', 'Uploads', 'Your Data', tableId = 'tab_entropy',
                   btnInputId = 'test_entropy', downId = 'down_entropy', data = data)
      }
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_entropy, format = 'html') %>% 
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
          downloadButton(outputId = 'entropyFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'entropy')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'entropy')
    print(w)
  })
}



bfun_entropy <- function(d,i) {
  dat <- d[i,]
  
  if(ncol(dat) == 1) {
    return(doubleEntropy(dat))
  } else if(any(lapply(dat, mean) == 1)) {
    return(0)
  } else {
    return(mean(apply(dat, 2, doubleEntropy)))
  }
}








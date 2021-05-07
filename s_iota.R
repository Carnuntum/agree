
other_iota <- tabItem(
  tabName = 'other_iota',
  fluidRow(
    fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    id = 'iotaDocum',
                    width = 12,
                    style = measure_title_style,
                    h3("Iota")
                  )
  )),
  hidden(
    div(id = 'iotaDocumBox',
        fluidRow(class = 'documRow',
                 column(width = 10,
                        offset = 1,
                        style = 'padding-left: 0px; padding-right: -5px;',
                        box(title = iota_docum_text,
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
      uiOutput('ui_iota')
    ))),
  column(
    width = 5,
    box(
      width = NULL,
      fileInput(inputId = 'iotaInput',
                label = 'Browse for .csv files'),
      h5('Number of rated variables'),
      numericInput(
        inputId = 'iotaNumInput',
        value = 1,
        label = '',
        min = 1
      ),
      h5('Choose weighting method'),
      div(
        class = 'selectInputStyle',
        selectInput(
          inputId = 'iota_scale',
          label = '',
          choices = c('quantitative',
                      'nominal')
        ),
        style = centerText
      ),
      actionButton(inputId = 'iotaRun',
                   label = 'calculate'),
      style = centerText
    ),
    column(
      width = 12,
      fluidRow(
        class = 'style_valuebox_OUTPUT_cyan',
        
        width = 12,
        shinyBS::popify(valueBoxOutput(outputId = 'iota', width = NULL), 
                        title = 'What means what',
                        content = paste0('<li>', names(iota_output_description),
                                         ' = ',
                                         as.character(iota_output_description), '</li>',
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
iotaOut <- function(input, output, data, scope = F) {
  
  tryCatch({
    
    data <- na.omit(data)
    numVars <<- if(ncol(data) == input$iotaNumInput) {ncol(data)} else {
      ncol(data)/input$iotaNumInput
    }
    print(numVars)
    choice <- input$iota_scale
    
    warning_handler(if(numVars%%1 != 0) {stop('cannot create equal number of rating matrices.
                                                           did you specifiy the correct number of variables?')})
    
    warning_handler(if(any(dim(data) == 0)) {stop('cannot create rating matrices. too many NAs?')})
    
    
    splitData <- function(data, numVars) {
      datList <- list()
      if(numVars == 1) {
        datList[[1]] <- data
      } else {
        begin <- 1
        end <- numVars
        for(i in seq_len(ncol(data)/numVars)) {
          datList[[i]] <- data[,begin:end]
          begin <- end + 1
          end <- end + numVars
        }
      }
      return(datList)
    }
    
    datList <- warning_handler(splitData(data, numVars))
    
    vals_iota <- list('vals' = warning_handler(irr::iota(datList, scaledata = if(is.null(choice)) {'quantitative'}
                                                         else {choice})),
    'warn' = msg)
    
    l_iota <<- lapply(vals_iota$vals, as.data.frame)
    
    class(vals_iota$vals) <- 'list'
    
    vals_iota$vals <- as.list(unlist(vals_iota$vals))
    
    ci <- warning_handler(makeCi(as.data.frame(datList), bfun))
    
    vals_iota$vals$lb <- ci[1]
    vals_iota$vals$ub <- ci[2]
    
    d_iota <- t(as.data.frame(vals_iota$vals))
    
    
    if(scope == F) {
      set_tabset(output, 'ui_iota', id = 'id_iota', tableId = 'tab_iota', btnInputId = 'test_iota',
                 downId = 'down_iota', data = data)
    } else {
      set_tabset(output, 'ui_iota', id = 'id_iota', 'Uploads', 'Your Data', tableId = 'tab_iota',
                 btnInputId = 'test_iota', downId = 'down_iota', data = data)
    }
    
    output$iota <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_iota, format = 'html') %>% 
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
          downloadButton(outputId = 'iotaFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'iota')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'iota')
    print(w)
  })
}



bfun <- function(d,i) {
  dat <- d[i,]
  
  datList <- list()
  
  begin <- 1
  end <- numVars
  for(i in seq_len(ncol(dat)/numVars)) {
    datList[[i]] <- dat[,begin:end]
    begin <- end + 1
    end <- end + numVars
  }
  return(irr::iota(datList)$value)
}








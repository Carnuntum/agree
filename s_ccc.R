source('text.R')

CCC <- tabItem(
  tabName = 'CCC',
  useShinyjs(),
  fluidRow(
    column(
      width = 10,
      offset = 1,
      style = 'padding-left: 0px; padding-right: -5px;',
      box(
        width = NULL,
        style = 'text-align:center; padding: 30px;',
        h3(id = 'cccDocum',
           "Concordance Correlation Coefficient")
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
          height = '131px',
          p(file_upload_text),
          style = 'text-align: center;'
        )
      ),
      fluidRow(
        box(
          width = NULL,
          height = '131px',
          p(file_struct_text),
          look_down,
          style = 'text-align: center;'
        )
      )
    ),
    column(
      width = 5,
      box(
        width = NULL,
        #custom js to change upload complete text
        fileInput(
          inputId = 'cccInput',
          label = 'Browse for .csv files',
          accept = ".csv"
        ),
        style = 'text-align: center;',
        div(
          class = 'checkbox_button_css',
          h5('CI Options'),
          checkboxGroupButtons(
            inputId = 'cccChoice',
            choices = c('z-transform', 'asymptotic'),
            status = 'cccbtn1',
            justified = T,
            individual = F,
            size = 'n',
            direction = 'horizontal'
          )
        ),
        actionButton(inputId = 'cccRun',
                     label = 'calculate')
      )
    )
  ),
  fluidRow(
    class = 'tabStyle',
    column(
      width = 5,
      offset = 1,
      style = 'padding: 0px;',
      uiOutput('ui_ccc'),
    ),
    column(width = 5,
           fluidRow(
             class = 'style_valuebox_CCC_cyan',
             column(width = 12,
                    valueBoxOutput(outputId = 'ccc1',
                                   width = NULL))
           ))
  ),
  fluidRow(
    class = 'tabStyle',
    column(
      width = 8,
      offset = 2,
      style = 'padding: 0;',
      shinyjs::hidden(
        div(id = 'div_plot_ccc',
            box(
              title = 'Bland Altman Plot',
              width = NULL,
              plotOutput('plot_ccc'),
              downloadButton('down_plot_ccc', style = 'margin: 20px;')
            ),
            style = 'text-align: center;'
        )
      )
    )
  ),
  fluidRow(column(width = 12,
                  style = 'height: 250px;'))
)

cccMainOut <- function(input, output, data) {
  
  tryCatch({
    test <- cccMain(input, output, data)
    
    l_ccc <<- lapply(test, as.data.frame)
    
    d_ccc <- t(data.frame(
      'Rho' = round(as.numeric(test$rho.c[1]), 3),
      'Lower CI' = round(as.numeric(test$rho.c[2]), 3), 
      'Upper CI' = round(as.numeric(test$rho.c[3]), 3), 
      'Scale Shift' = round(as.numeric(test$s.shift), 3), 
      'Location Shift' = round(as.numeric(test$l.shift), 3), 
      'Bias Correction' = round(as.numeric(test$C.b), 3)
    ))
    
  
  shinyjs::show('div_plot_ccc', anim = T)
  output$plot_ccc <- renderPlot({
    blaltm <<- BlandAltmanLeh::bland.altman.plot(group1 = data[,1], group2 = data[,2],
                                      graph.sys = 'ggplot2')
    blaltm
  })
  
  output$ccc1 <- renderValueBox({
    tryCatch({
      valueBox(
        value = p(HTML(
          kableExtra::kable(d_ccc, format = 'html') %>% 
            kableExtra::kable_styling('basic'),
          
        ),
        div(
          downloadButton(outputId = 'cccFullDown',
                         label = 'Full Results'),
          style = 'text-align: center;'
        )),
        subtitle = ''
      )
    }, error = function(e) {
      print('ccc_out_e')
      print(e)
    }, warning = function(w) {
      print('ccc_out_w')
      print(w)
    })
  })
  
  }, error = function(e) {
    print('ccc_test_e')
    print(e)
  }, warning = function(w) {
    print('ccc_test_w')
    print(w)
  })
}
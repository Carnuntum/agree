
other_icc <- tabItem(tabName = 'other_icc',
               useShinyjs(),
                    fluidRow(
                      column(
                        width = 10,
                        offset = 1,
                        style = 'padding-left: 0px; padding-right: -5px;',
                        box(
                          id = 'iccDocum',
                          width = NULL,
                          style = measure_title_style,
                          h3("Intraclass Correlation Coefficient")
                        ),
                        hidden(
                          div(id = 'iccDocumBox',
                              fluidRow(class = 'documRow',
                                       column(width = 12,
                                              offset = 0,
                                              box(title = icc_docum_text,
                                                  width = NULL,
                                                  style = measure_title_style)
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
                            p(file_upload_text),
                            style = 'text-align: center;'
                          )
                        ),
                        fluidRow(
                          box(
                            width = NULL,
                            p(file_struct_text),
                            look_down,
                            style = 'text-align: center;'
                          )
                        )
                      ),
                      column(
                        width = 5,
                        box(width = NULL,
                            #custom js to change upload complete text
                            fileInput(inputId = 'iccInput',
                                      label = 'Browse for .csv files',
                                      accept = ".csv"),
                            style = 'text-align: center;',
                            div(class = 'checkbox_button_css',
                                checkboxGroupButtons(inputId = 'iccChoices',
                                                     status = 'iccbtn1',
                                                     justified = T,
                                                     individual = F,
                                                     size = 'n',
                                                     choices = c('oneway', 'twoway'),
                                                     direction = 'horizontal'),
                                checkboxGroupButtons(inputId = 'iccChoices2',
                                                     status = 'iccbtn3',
                                                     justified = T,
                                                     individual = F,
                                                     size = 'n',
                                                     choices = c('single', 'average'),
                                                     direction = 'horizontal')),
                            actionButton(inputId = 'iccRun',
                                         label = 'calculate')
                        )
                      )
                    ),
                    fluidRow(class = 'tabStyle',
                             column(
                               width = 5, 
                               offset = 1,
                               style = 'padding: 0px;',
                               uiOutput('ui_icc')
                             ),
                             column(
                               width = 5,
                               dropMenu(
                                 div(id = 'iccDrop',
                                     fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                                              column(
                                                width = 12,
                                                valueBoxOutput(
                                                  outputId = 'icc1',
                                                  width = NULL)
                                                )
                                              )
                                     ),
                                 HTML(kableExtra::kable(t(icc_output_description)) %>% 
                                        kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri')),
                                 trigger = 'mouseenter',
                                 theme = 'translucent',
                                 placement = 'left-start'
                               )
                               
                             )
                    )
)


iccMainOut <- function(input, output, data, test = F) {
  
  test <- warning_handler(iccMain(input, output, data, test))
  
  tryCatch({
    
    
    l_icc <<- as.list(lapply(test, as.data.frame))
    
    d_icc <- t(data.frame(
      'Subjects' = test$subjects,
      'Raters' = test$raters,
      'Model' = test$model,
      'Type' = test$type,
      'Unit' = test$unit,
      'ICC Name' = test$icc.name,
      'Value' = test$value,
      'F Statistic' = test$Fvalue,
      'df-1' = test$df1,
      'df-2' = test$df2,
      'p-value' = test$p.value,
      'Lower CI' = test$lbound,
      'Upper CI' = test$ubound
    ))
    
    
    output$icc1 <- renderValueBox({
      tryCatch({
        valueBox(
          value = p(HTML(
            kableExtra::kable(d_icc, format = 'html') %>% 
              kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri'),
            
          ),
          div(
            downloadButton(outputId = 'iccFullDown',
                           label = 'Full Results'),
            style = 'text-align: center;'
          )
          ),
          subtitle = ''
        )
      }, error = function(e) {
        print('tab_function_icc_e_inner')
        print(e)
        iccErrOut(output)
      }, warning = function(w) {
        print('tab_function_icc_w_inner')
        print(w)
        iccErrOut(output)
      })
    })
  }, error = function(e) {
    print('tab_function_icc_e_outer')
    print(e)
    iccErrOut(output)
  }, warning = function(w) {
    print('tab_function_icc_w_outer')
    print(w)
    iccErrOut(output)
  })
}

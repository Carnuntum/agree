

icc <- tabItem(tabName = 'icc',
               useShinyjs(),
                    fluidRow(
                      column(
                        width = 10,
                        offset = 1,
                        style = 'padding-left: 0px; padding-right: -5px;',
                        box(
                          width = NULL,
                          style = 'text-align:center; padding: 30px;',
                          h3("Interrater Correlation")
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
                                checkboxGroupButtons(inputId = 'iccChoices1',
                                                     status = 'iccbtn2',
                                                     justified = T,
                                                     individual = F,
                                                     size = 'n',
                                                     choices = c('consistency', 'agreement'),
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
                               fluidRow(class = 'style_valuebox_ICC_cyan',
                                        column(
                                          width = 12,
                                          valueBoxOutput(
                                            outputId = 'icc1',
                                            width = NULL)))
                               
                             )
                    )
)


iccMainOut <- function(input, output, data, test = F) {
  
  test <- iccMain(input, output, data, test)
  
  
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
      'Null Hypothesis' = test$r0,
      'F Statistic' = test$Fvalue,
      'df-1' = test$df1,
      'df-2' = test$df2,
      'p-value' = test$p.value,
      'Confidence Level' = test$conf.level,
      'Lower CI' = test$lbound,
      'Upper CI' = test$ubound
    ))
    
    
    output$icc1 <- renderValueBox({
      tryCatch({
        valueBox(
          value = p(HTML(
            kableExtra::kable(d_icc, format = 'html') %>% 
              kableExtra::kable_styling('basic'),
            
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
lvlInterval <- list('NONE', 
                    'GAUSSIAN',
                    'LAPLACE',
                    't', 'GAMMA',
                    'EMPIRICAL')
lvlRatio <- list('BETA',
                  'KUMARASWAMY')


other_omega <- tabItem(
  tabName = 'other_omega',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px;',
                  box(
                    id = 'omegaDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("Sklars Omega")
                  ),
                  hidden(
                    div(id = 'omegaDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = omega_docum_text,
                                            width = NULL,
                                            style = 'text-align: justify/left/right/center;')
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
        uiOutput('ui_omega')
      ))),
    column(
      width = 5,
      box(
        width = NULL,
        fileInput(inputId = 'omegaInput',
                  label = 'Browse for .csv files'),
        actionButton(inputId = 'omegaRun',
                     label = 'calculate'),
        style = centerText,
        div(class = 'checkbox_button_css',
           h5('Scale Level'),
           checkboxGroupButtons(inputId = 'scaleChoice',
                                status = 'omegabtn1',
                                justified = T,
                                individual = F,
                                size = 'n',
                                choices = c('nominal', 'ordinal', 'interval', 'ratio'),
                                direction = 'horizontal'),
           h5('Method For Confidence Interval'),
           checkboxGroupButtons(inputId = 'bootChoice',
                                status = 'omegabtn2',
                                justified = T,
                                individual = F,
                                size = 'n',
                                choices = c('none', 'bootstrap', 'asymptotic'),
                                direction = 'horizontal'),
           div(class = 'selectInputStyle',
               selectInput(inputId = 'distChoice',
                           label = 'Distribution',
                           choices = list(`Interval Scale` =
                                         lvlInterval,
                                       `Ratio Scale` =
                                         lvlRatio
                                       )
                           )
               )
           )
      ),
      column(
        width = 12,
        fluidRow(
          class = 'style_valuebox_OUTPUT_cyan',
          
          width = 12,
          shinycssloaders::withSpinner(
            type = getOption("spinner.type", default = 7),
            color = getOption("spinner.color", default = "#3c8dbc"),
            proxy.height = '50px',
            
            ui_element = shinyWidgets::dropMenu(
              div(id = 'omegaDrop',
                  fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                           column(
                             width = 12,
                             valueBoxOutput(outputId = 'omega', width = NULL)
                           )
                  )
              ),
              HTML(kableExtra::kable(t(omega_output_description)) %>% 
                     kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri')),
              trigger = 'mouseenter',
              theme = 'translucent',
              placement = 'left-start'),
            
            id = 'omegaSpinner'
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
omegaOut <- function(input, output, data, scope = F) {
  
  tryCatch({
    
    
    output$omega <- renderValueBox({
      
      tryCatch({
        isolate({
          vals_omega <- list('vals' = warning_handler(sklarsomega::sklars.omega(data,
                                                                                level = input$scaleChoice,
                                                                                confint = input$bootChoice,
                                                                                control = list(
                                                                                  bootit = 10,
                                                                                  dist = tolower(input$distChoice)
                                                                                  )
                                                                                )
                                                      ),
                                    'warn' = msg)
          
          if(is.null(vals_omega$vals$coefficients[1])) {
            stop(msg)
          }
        
          vals_omega$vals$call <- NULL
          
          l_omega <<- warning_handler(lapply(vals_omega$vals, as.data.frame))
          
          d_omega <- data.frame(
            Coefficients = vals_omega$vals$coefficients[1], #inter-rater value
            Confint = vals_omega$vals$confint,
            lci = if(input$bootChoice != 'none') {confint(vals_omega$vals)[1,][1]} else {'none'},
            uci = if(input$bootChoice != 'none') {confint(vals_omega$vals)[1,][2]} else {'none'}
          )
          
          d_omega <- t(as.data.frame(d_omega))
        
        })
        
        
        if(scope == F) {
          set_tabset(output, 'ui_omega', id = 'id_omega', tableId = 'tab_omega', btnInputId = 'test_omega',
                     downId = 'down_omega', data = data)
        } else {
          set_tabset(output, 'ui_omega', id = 'id_omega', 'Uploads', 'Your Data', tableId = 'tab_omega',
                     btnInputId = 'test_omega', downId = 'down_omega', data = data)
        }
      }, error = function(e) {
        invalid_data(output, 'omega')
        print(e)
      }, warning = function(w) {
        invalid_data(output, 'omega')
        print('warning happend')
        print(w)
      })
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_omega, format = 'html') %>% 
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
          downloadButton(outputId = 'omegaFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'omega')
    print(e)
  }, warning = function(w) {
    print(c('warn', msg))
    invalid_data(output, 'omega')
    print(w)
  })
}










# other_omega <- tabItem(tabName = 'other_omega',
#                useShinyjs(),
#                fluidRow(
#                  column(
#                    width = 10,
#                    offset = 1,
#                    style = 'padding-left: 0px; padding-right: -5px;',
#                    box(
#                      width = NULL,
#                      style = measure_title_style,
#                      h3("Sklars Omega")
#                    )
#                  )
#                ),
#                fluidRow(
#                  column(
#                    width = 5,
#                    offset = 1,
#                    fluidRow(
#                      box(
#                        width = NULL,
#                        height = '131px',
#                        p(file_upload_text),
#                        style = 'text-align: center;'
#                      )
#                    ),
#                    fluidRow(
#                      box(
#                        width = NULL,
#                        height = '131px',
#                        p(file_struct_text),
#                        look_down,
#                        style = 'text-align: center;'
#                      )
#                    )
#                  ),
#                  column(
#                    width = 5,
#                    box(width = NULL,
#                        #custom js to change upload complete text
#                        fileInput(inputId = 'omegaInput',
#                                  label = 'Browse for .csv files',
#                                  accept = ".csv"),
#                        style = 'text-align: center;',
#                        div(class = 'checkbox_button_css',
#                            h5('Scale Level'),
#                            checkboxGroupButtons(inputId = 'omegaChoices',
#                                                 status = 'omegabtn1',
#                                                 justified = T,
#                                                 individual = F,
#                                                 size = 'n',
#                                                 choices = c('nominal', 'ordinal', 'interval', 'ratio'),
#                                                 direction = 'horizontal'),
#                            h5('Method For Confidence Interval'),
#                            checkboxGroupButtons(inputId = 'omegaChoices1',
#                                                 status = 'omegabtn2',
#                                                 justified = T,
#                                                 individual = F,
#                                                 size = 'n',
#                                                 choices = c('none', 'bootstrap', 'asymptotic'),
#                                                 direction = 'horizontal'),
#                            div(class = 'selectInputStyle',
#                                selectInput(inputId = 'omegaChoices2',
#                                            label = 'Distribution',
#                                            choices = list(`Interval Scale` = 
#                                                          lvlInterval,
#                                                        `Ratio Scale` = 
#                                                          lvlRatio
#                                                        )
#                                            )
#                                )
#                            ),
#                        actionButton(inputId = 'omegaRun',
#                                     label = 'calculate')
#                    )
#                  )
#                ),
#                fluidRow(class = 'tabStyle',
#                         column(
#                           width = 5, 
#                           offset = 1,
#                           style = 'padding: 0px;',
#                           uiOutput('ui_omega')
#                         ),
#                         column(
#                           width = 5,
#                           fluidRow(class = 'style_valuebox_OUTPUT_cyan',
#                                    column(
#                                      width = 12,
#                                      shinycssloaders::withSpinner(
#                                        type = getOption("spinner.type", default = 7),
#                                        color = getOption("spinner.color", default = "#3c8dbc"),
#                                        proxy.height = '200px',
#                                        valueBoxOutput(
#                                          outputId = 'omega1',
#                                          width = NULL),
#                                        id = 'omegaSpinner'
#                                      )
#                                    )
#                           )
#                         )
#                )
# )
# 
# omegaMainOut <- function(input, output, data) {
#   
#     tryCatch({
#       
#       output$omega1 <- renderValueBox({
#         choices <- c(input$omegaChoices, input$omegaChoices1, input$omegaChoices2)
#           
#         if(length(choices) != 3) {tooManyOmega()}
#         else {
#           test <- warning_handler(sklarsomega::sklars.omega(data,
#                                                             level = choices[1],
#                                                             confint = choices[2],
#                                                             verbose = T,
#                                                             control = list(
#                                                               bootit = 1000,
#                                                               dist = choices[3]
#                                                             )
#                                                             ))
#           test$call <- as.character(test$call)
#           
#           vals <- data.frame(
#             Coefficients = test$coefficients[1], #inter-rater value
#             Confint = test$confint,
#             Convergence = test$convergence,
#             meth = test$method,
#             lci = if(choices[2] != 'none') {confint(test)[1,][1]} else {'none'},
#             uci = if(choices[2] != 'none') {confint(test[1,][2])} else {'none'}
#           )
#           l_omega <<- as.list(lapply(test, as.data.frame))
#         }
#         
#           valueBox(
#             value = p(HTML(
#               kableExtra::kable(t(vals), format = 'html') %>% 
#                 kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri'),
#               
#             ),
#             div(
#               if(!is.null(msg)) {
#                 p(HTML(paste0(
#                   circleButton(inputId = 'warningButton',
#                                icon = icon("exclamation"),
#                                size = 's'),
#                   br()
#                 )))
#               },
#               style = centerText
#             ),
#             div(
#               downloadButton(outputId = 'omegaFullDown',
#                              label = 'Full Results'),
#               style = centerText
#             )),
#             subtitle = ''
#             
#           )
#       })
#       
#     }, error = function(e) {
#       invalid_data(output, 'omega1')
#       print(e)
#     }, warning = function(w) {
#       invalid_data(output, 'omega1')
#       print(w)
#     })
# }

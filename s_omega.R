lvlInterval <- list('NONE', 
                    'GAUSSIAN',
                    'LAPLACE',
                    't', 'GAMMA',
                    'EMPIRICAL')
lvlRatio <- list('BETA',
                  'KUMARASWAMY')


omega <- tabItem(tabName = 'omega',
               useShinyjs(),
               fluidRow(
                 column(
                   width = 10,
                   offset = 1,
                   style = 'padding-left: 0px; padding-right: -5px;',
                   box(
                     width = NULL,
                     style = measure_title_style,
                     h3("Sklars Omega")
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
                       fileInput(inputId = 'omegaInput',
                                 label = 'Browse for .csv files',
                                 accept = ".csv"),
                       style = 'text-align: center;',
                       div(class = 'checkbox_button_css',
                           h5('Scale Level'),
                           checkboxGroupButtons(inputId = 'omegaChoices',
                                                status = 'omegabtn1',
                                                justified = T,
                                                individual = F,
                                                size = 'n',
                                                choices = c('nominal', 'ordinal', 'interval', 'ratio'),
                                                direction = 'horizontal'),
                           h5('Method For Confidence Interval'),
                           checkboxGroupButtons(inputId = 'omegaChoices1',
                                                status = 'omegabtn2',
                                                justified = T,
                                                individual = F,
                                                size = 'n',
                                                choices = c('none', 'bootstrap', 'asymptotic'),
                                                direction = 'horizontal'),
                           div(class = 'selectInputStyle',
                               selectInput(inputId = 'omegaChoices2',
                                           label = 'Distribution',
                                           choices = list(`Interval Scale` = 
                                                         lvlInterval,
                                                       `Ratio Scale` = 
                                                         lvlRatio
                                                       )
                                           )
                               )
                           ),
                       actionButton(inputId = 'omegaRun',
                                    label = 'calculate')
                   )
                 )
               ),
               fluidRow(class = 'tabStyle',
                        column(
                          width = 5, 
                          offset = 1,
                          style = 'padding: 0px;',
                          uiOutput('ui_omega')
                        ),
                        column(
                          width = 5,
                          fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                                   column(
                                     width = 12,
                                     shinycssloaders::withSpinner(
                                       type = getOption("spinner.type", default = 7),
                                       color = getOption("spinner.color", default = "#3c8dbc"),
                                       proxy.height = '200px',
                                       valueBoxOutput(
                                         outputId = 'omega1',
                                         width = NULL),
                                       id = 'omegaSpinner'
                                     )
                                   )
                          )
                        )
               )
)

omegaMainOut <- function(input, output, data) {
  
  tryCatch({
    
    output$omega1 <- renderValueBox({
      
      isolate({
        test <- omegaMain(input, output, data)
        test$call <- NULL
        omegaMessages <<- as.character(test)
        choices <- c(input$omegaChoices, input$omegaChoices1, input$omegaChoices2)
        
        AIC = test$AIC
        BIC = test$BIC
        Coefficients = sapply(test$coefficients, list)
        Confint = test$confint
        Convergence = test$convergence
        conv.iter = test$iter
        msg = test$message
        meth = test$method
        np = test$mpar
        n = test$npar
        val = test$value
        
        l_omega <<- as.list(lapply(test, as.data.frame))
      })
      
      tryCatch({
        
        valueBox(
          value = p(HTML(
            paste0(
              'AIC: ', AIC, br(),
              'BIC: ', BIC, br(),
              'coeff-inter: ', Coefficients$inter, br(),
              'coeff-mu: ', Coefficients$mu, br(),
              'coeff-sigma: ', Coefficients$sigma, br(),
              'confint: ', Confint, br(),
              'convergence: ', Convergence, br(),
              'convergence iteration: ', conv.iter ,br(),
              'method: ', meth, br(),
              'n of marginal parameters: ', np, br(),
              'total parameters: ', n, br(),
              'value: ', val, br(),
              'warnings: ', if(exists("globalOmegaWarn")) {globalOmegaWarn}, br(),
              br(),
              div(
                downloadButton(outputId = 'omegaResDown',
                               label = 'full result'),
                style = 'text-align: center;'),
              br()
              
            )
          ), style = 'color: white;'),
          subtitle = ''
          
        )
      }, error = function(e) {
        print(e)
        print('1')
        globalOmegaWarn <<- (conditionMessage(e))
        omegaErrOut(output)
      }, warning = function(w) {
        print('2')
        print(w)
        globalOmegaWarn <<- (conditionMessage(w))
        omegaErrOut(output)
      })
    })
  }, error = function(e) {
    print('3')
    print(e)
    globalOmegaWarn <<- (conditionMessage(e))
    omegaErrOut(output)
  }, warning = function(w) {
    print('4')
    print(w)
    globalOmegaWarn <<- (conditionMessage(w))
    omegaErrOut(output)
  })
}
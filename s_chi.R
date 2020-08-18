#-------------------------------------------------------------------------------
#'*----------------------------- CHI TABITEM ----------------------------------*
#-------------------------------------------------------------------------------

chi <- tabItem(useShinyjs(), tabName = 'chi',
               fluidRow(
                 style = 'text-align: center; padding: 30px;',
                 h3('Chi-square Test for 2 Dimensional Contingency Tables')
               ),
               fluidRow(
                 column(
                   width = 5, 
                   offset = 1,
                   box(width=NULL,
                       height = '100px',
                       h5('On the right side you can \
                         upload your 2 by 2 contingency data \
                         via a .csv file.'),
                       h5('The file has to have the \
                         following structure:'))),
                 column(
                   width = 5,
                   box(width = NULL, height = '100px',
                       fileInput(inputId = 'chiInput',
                                 label = 'Browse for .csv files')))
               ),
               fluidRow(
                 column(
                   width = 5, 
                   offset = 1,
                   box(title = 'Data example', 
                       width = NULL, 
                       height = '250px',
                       tableOutput('expChi'),
                       actionButton(
                         inputId = 'chiTest',
                         label = 'test run',
                         style = 'float:right;'))),
                 column(
                   width = 5,
                   fluidRow(class = 'chiValueBox1',
                            column(
                              width = 12,
                              style = 'text-align: center;',
                              valueBoxOutput(
                                outputId = 'chiCor',
                                width = NULL))),
                   fluidRow(class = 'chiValueBox2',
                            column(
                              width = 12, 
                              style = 'text-align: center;',
                              valueBoxOutput(
                                outputId = 'chiUncor',
                                width = NULL))),
                   fluidRow(class = 'contCoeffValueBox',
                            column(
                              width = 12,
                              style = 'text-align: center;',
                              valueBoxOutput(
                                outputId = 'contingencyCoeff',
                                width = NULL)))
                   
                 )
               )
)

#-------------------------------------------------------------------------------
#'*-------------------------- CHI SERVER LOGIC --------------------------------*
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#'*-------------------- CHI EXAMPLE TABLE DATA --------------------------------*
#-------------------------------------------------------------------------------
chiExampleTable <- function(input, output) {
  output$expChi <- function() {
    kable(chiTableExp, format = 'html') %>%
      kable_styling('basic')
  }
}

#-------------------------------------------------------------------------------
#'*---------------- DEFAULT INFORMATION FOR CHI OUTPUT BOXES ------------------*
#-------------------------------------------------------------------------------
chiOutDefault <- function(inp, out) {
  output$chiUncor <- renderValueBox({
    if (is.null(input$chiInput)) {
      valueBox(value = h5('Statistic uncorrected'), '')
    }
  })
  output$chiCor <- renderValueBox({
    if (is.null(input$chiInput)) {
      valueBox(value = h5('Statistic corrected'), '')
    }
  })
  
  output$contingencyCoeff <- renderValueBox({
    if (is.null(input$chiInput)) {
      valueBox(value = h5('Contingency Coefficient C'), '')
    }
  })
}



#-------------------------------------------------------------------------------
#'*------------------ CALCULATED OUTPUT FOR CHI TEST --------------------------*
#-------------------------------------------------------------------------------
#chi output for corrected test
chiOut <- function(input, output, data) {
  
  test <- chisq.test(data)
  chiValue <- test$statistic
  p <- test$p.value
  par <- test$parameter
  coeffC <- ContCoef(data)
  
  output$chiCor <- renderValueBox({
    valueBox(
      value = p(HTML(
        paste0(
          'X',
          tags$sup('2'),' = ',
          round(as.numeric(chiValue), 3),
          '<br/>',
          'df = ',
          par,
          '<br/>',
          if (round(p, 3) == 0) {
            'p < 0.001'
          }
          else {
            paste('p = ', round(p, 3))
          }
        )
      ),
      style = 'font-size: 50%; text-align:center;'),
      p('corrected', style = 'text-align:center;')
    )
  })
  
  #chi output for uncorrected test
  output$chiUncor <- renderValueBox({
    test <- chisq.test(data, correct = F)
    
    valueBox(
      value = p(HTML(
        paste0(
          'X',
          tags$sup('2'),' = ',
          round(as.numeric(test$statistic), 3),
          '<br/>',
          'df = ',
          test$parameter,
          '<br/>',
          if (round(test$p.value, 3) == 0) {
            'p < 0.001'
          }
          else {
            paste('p = ', round(test$p.value, 3))
          }
        )
      ),
      style = 'font-size: 50%; text-align:center;'),
      p('uncorrected', style = 'text-align:center;')
    )
  })
  
  output$contingencyCoeff <- renderValueBox({
    
    valueBox(
      value = p(HTML(
        paste0(
          'Contingency Coefficient',
          ' = ',
          round(as.numeric(coeffC), 3),
          '<br/>'
        )
      ),
      style = 'font-size: 50%; text-align:center;
               margin-top: 30px;'),
      subtitle =  p('') #here may come Cmax <- need number of categories in dataset
    )
  })
}





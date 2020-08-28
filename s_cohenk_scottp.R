#-------------------------------------------------------------------------------
#'*------------------------- COHENS KAPPA TABITEM -----------------------------*
#-------------------------------------------------------------------------------



cohenk <- tabItem(useShinyjs(),tabName = 'cohenk',
                  
    fluidRow(
      style = 'text-align: center; padding: 30px;',
      h3("Cohen's Kappa And Scott's Pi For 2 Raters And N Categories")
    ),
    fluidRow(
      column(
        width = 5, 
        offset = 1,
        box(width=NULL,
            height = '100px',
            h5('On the right side you can \
                         upload your data \
                         via a .csv file.'),
            h5('The file has to have the \
                         following structure:'))),
      column(
        width = 5,
        box(width = NULL, height = '100px',
            fileInput(inputId = 'kappaInput',
                      label = 'Browse for .csv file')))
    ),
    fluidRow(
      column(
        width = 5, 
        offset = 1,
        box(title = 'Data example', 
            width = NULL, 
            tableOutput('expKappa'),
            actionButton(
              inputId = 'kappaTest',
              label = 'test run',
              style = 'float:right;'))),
      column(
        width = 5,
        fluidRow(class = 'style_valuebox_PA_cyan',
                 column(
                   width = 12,
                   style = 'text-align: center;',
                   valueBoxOutput(
                     outputId = 'kappa1',
                     width = NULL))),
        fluidRow(class = 'style_valuebox_PA_cyan',
                 column(
                   width = 12, 
                   style = 'text-align: center;',
                   valueBoxOutput(
                     outputId = 'pi1',
                     width = NULL))),
        fluidRow(class = 'style_valuebox_PA_cyan',
                 column(
                   width = 12, 
                   style = 'text-align: center;',
                   valueBoxOutput(
                     outputId = 'fleissKappa',
                     width = NULL))),
        
        
      )
    )
  )

#-------------------------------------------------------------------------------
#'*-------------------------- KAPPA SERVER LOGIC --------------------------------*
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#'*-------------------- KAPPA EXAMPLE TABLE DATA ------------------------------*
#-------------------------------------------------------------------------------
# kappaExampleTable <- function(inp, out) {
#   output$expKappa <- function() {
#     kable(kappaTableExample, format = 'html') %>%
#       kable_styling('basic')
#   }
# }




#-------------------------------------------------------------------------------
#'*-------------- DEFAULT INFORMATION FOR KAPPA OUTPUT BOXES ------------------*
#-------------------------------------------------------------------------------
kappaOutDefault <- function(inp, out) {
  output$kappa1 <- renderValueBox({
    if (is.null(input$kappaInput)) {
      valueBox(value = h5(''), subtitle = "Cohen's Kappa")
    }
  })
  output$pi1 <- renderValueBox({
    if (is.null(input$kappaInput)) {
      valueBox(value = h5(''), "Scott's Pi")
    }
  })
  output$fleissKappa <- renderValueBox({
    if (is.null(input$kappaInput)) {
      valueBox(value = h5(''), "Fleiss' Kappa")
    }
  })
}

#-------------------------------------------------------------------------------
#'*------------------ CALCULATED OUTPUT FOR CHI TEST --------------------------*
#-------------------------------------------------------------------------------

kappaOut <- function(input, output, data) {
  
  if(ncol(data) == 2) {
    kap <- kappa2(data)
    kapVal <- kap$value
    kapP <- kap$p.value
    kapZ <- kap$statistic
    
    scott <- spi(data, weight = 'unweighted')
    scottVal <- scott$est
  
  
    output$kappa1 <- renderValueBox({
      valueBox(
        value = 
          p(HTML(
            paste0(
              'K = ', round(as.numeric(kapVal), 3),
              br(),
              'z = ', round(as.numeric(kapZ), 3),
              '<br/>',
              if(round(as.numeric(kapP), 3) < 0.001) {'p < 0.001'}
              else {paste0('p = ', round(as.numeric(p), 3))}
              
            ),
          ),
          style = 'font-size: 50%; text-align:center;'),
        subtitle = "Cohen's Kappa"
      )
    })
    
    output$pi1 <- renderValueBox({
      valueBox(
        value = 
          p(HTML(
            paste0(
              'Pi = ', round(scottVal, 3)
            ),
          ),
          style = 'font-size: 50%; text-align:center;
                   margin-top: 30px;'),
        subtitle = "Scott's Pi"
      )
    })
  }
  else {
    f <- kappam.fleiss(data)
    fVal <- f$value
    fStat <- f$statistic
    fleissP <- f$p.value
    
    output$fleissKappa <- renderValueBox({
      valueBox(
        value = 
          p(HTML(
            paste0(
              "Fleiss' Kappa = ", round(fVal, 3),
              br(),
              'z-value = ', round(fStat, 3),
              br(),
              if(round(fleissP, 3) < 0.001) {'p-value < 0.001'}
              else {paste0('p-value = ', round(as.numeric(p), 3))}
            )
          ))
      )
    })
  }
}
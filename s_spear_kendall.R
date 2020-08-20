source('src_functions.R')

ordinalRank <- tabItem(tabName = 'spear',
                        fluidRow(
                          column(
                            width = 10,
                            offset = 1,
                            style = 'padding-left: 0px; padding-right: -5px;',
                            box(
                              width = NULL,
                              style = 'text-align:center; padding: 30px;',
                              h3("Spearman's Rho for 2 Raters and Kendall's W 
                                 for N Raters")
                              )
                            )
                          ),
                        fluidRow(
                          column(
                            width = 5, 
                            offset = 1,
                            style = 'padding: 0px;',
                            box(
                              width = NULL,
                              height = '100px',
                              h5('On the right side you can upload your data 
                                 via a .csv file.'),
                              h5('The file has to have the following 
                                 structure:'),
                              style = 'text-align: center;')
                            ),
                          column(
                            width = 5,
                            box(width = NULL,
                                fileInput(inputId = 'ordinalInput',
                                          label = 'Browse for .csv files'),
                                style = 'text-align: center;',
                                actionButton(inputId = 'spearRun',
                                             label = "Spearman's Rho"),
                                actionButton(inputId = 'kendRun',
                                             label = "Kendall's W")))
                        ),
                        fluidRow(
                          column(
                            width = 5, 
                            offset = 1,
                            style = 'padding: 0px;',
                            box(title = 'Data example', 
                                width = NULL,
                                tableOutput('expSpear'),
                                actionButton(
                                  inputId = 'spearTest',
                                  label = 'test run',
                                  style = 'float:right;'),
                                style = 'text-align: center;')),
                          column(
                            width = 5,
                            fluidRow(class = 'style_valuebox_SPEAR_cyan',
                                     column(
                                       width = 12,
                                       style = 'text-align: center;',
                                       valueBoxOutput(
                                         outputId = 'spear1',
                                         width = NULL))),
                            fluidRow(class = 'style_valuebox_SPEAR_cyan',
                                     column(
                                       width = 12,
                                       style = 'text-align: center;',
                                       valueBoxOutput(
                                         outputId = 'kendall1',
                                         width = NULL)))
                            
                          )
                        )
)

#-------------------------------------------------------------------------------
#'*---------------------- SPEARMAN RHO SERVER LOGIC ---------------------------*
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#'*------------------- SPEARMAN RHO EXAMPLE TABLE DATA ------------------------*
#-------------------------------------------------------------------------------
spearExp <- function(input, output) {
output$expSpear <- function() {
  kable(spearmanTableExample, format = 'html') %>%
    kable_styling('basic')
  }
}

#-------------------------------------------------------------------------------
#'*------------------------- DEFAULT OUTPUT -----------------------------------*
#-------------------------------------------------------------------------------

spearOutDefault <- function(inp, out) {
  output$spear1 <- renderValueBox({
    
    valueBox(value = h4("Spearman's Rank Correlation Coefficient Rho",
                        style = 'text-align: center;
                                  padding: 15px;'), '')
    
  })
  
  output$kendall1 <- renderValueBox({
    
    valueBox(value = h4("Kendall's Concordance Coefficient W",
                        style = 'text-align: center;
                                  padding: 15px;'), '')
    
  })
}

#-------------------------------------------------------------------------------
#'*------------------------- CALCULATED OUTPUT --------------------------------*
#-------------------------------------------------------------------------------


spearOut <- function(input, output, data, method) {
  
  check <- F
  if(any(unlist(lapply(data, duplicated)))) {
    check <- T
  }
  
  test <- ordinals(data, method)
  
  stat <- round(as.numeric(test$statistic), 3)
  df <- test$parameter
  p <- round(as.numeric(test$p.value), 3)
  rho <- round(as.numeric(test$estimate), 3)
  W <- round(as.numeric(test$value), 3)
  
  output$spear1 <- renderValueBox({
    
    valueBox(
      value = h4("Spearman's Rank Correlation Coefficient Rho",
      style = 'text-align: center;
              padding-top: 10px;
              font-size: 25px;'),
      
      subtitle = 
        p(
          HTML(
            paste0(
              if(length(rho) == 0) {
                paste0('W: ', W)
              } else  {
                paste0('Rho: ', rho)
              },
              br(),
              if(!is.null(df)) {
                paste0('df: ', df, br())
              },
              'p-value: ', if(p < 0.001) {'< 0.001'}
                           else {p},
              '&emsp;', #inserting a tab (4 spaces)
              if(check) {
                circleButton(inputId = 'spear_p_value',
                             icon = icon("exclamation"),
                             size = 'xs')
              },
              br(),
              'test statistic: ', stat
            )
          ),
          style = 'text-align: left;
                  font-size: 25px;')
    )
  })
  
}


spear_no_exact_p <- function() {
  showModal(
    modalDialog(
      title = 'Warning!',
      'Exact p-value cannot be calculated with ties in dataset!',
      easyClose = T,
      style = 'text-align: center;'
    )
  )
}
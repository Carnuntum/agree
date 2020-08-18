iccMain <- tabItem(
  useShinyjs(),
  tabName = 'icc',
  fluidRow(column(width = 10, offset = 1,
                  style='text-align:center;',
                  h2('Intra Class Correlation'),
                  h5('Here you can choose one of the three possible ICC to calculate for your data'))),
    
  fluidRow(column(width = 5, offset = 1,
                  box(
                      width = NULL, p('Everything for intraclass correlation'),
                      style='text-align:center;',
                      actionButton(inputId = 'iccStart', label = 'start')))
    )
  )
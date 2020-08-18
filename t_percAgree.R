percentAgree <- tabItem(
  tabName = 'percAgree',
  fluidRow(style = 'text-align:center; padding: 30px;',
           h3('Percent Agreement for 2 by \
              2 or 2 by n contingency tables')
           ),
  fluidRow(
    column(
      width = 5,
      offset = 1,
      fluidRow(
        box(
          width = NULL,
          height = '105px',
          p(
            'On the right side you can upload your 2 by 2 or 2 by n data \
          via a .csv file and type the number of categories for which ratings \
          were applied'
          ),
          style = 'text-align: center;'
        )
      ),
      fluidRow(
        box(
          width = NULL,
          height = '105px',
          p('The data has to have the following structure:'),
          style = 'text-align: center;'
        )
      )
    )
    ,
    column(
      width = 5,
      box(
        width = NULL,
        height = '230px',
        fileInput(inputId = 'paInput',
                  label = 'Browse for .csv files'),
        textInput(
          inputId = 'txtInput_paCategories',
          label = 'Categories',
          value = 0
        ),
        actionButton(
          inputId = 'paRun',
          label = 'run',
          style = 'margin-left: 40%;'
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 5,
      offset = 1,
      style = 'padding: 0px;',
      box(
        title = 'Data Example',
        width = NULL,
        height = '450px',
        tableOutput('expPA'),
        actionButton(
          inputId = 'paTest',
          label = 'test run',
          style = 'float: right;'
        )
      )
    ),
    
    column(width = 5,
           fluidRow(class = 'percAgrValueBox1',
             column(
               width = 12,
               valueBoxOutput(outputId = 'paTotal',
                              width = NULL)
             )
           ),
           fluidRow(class = 'percAgrValueBox2',
             column(
               width = 12,
               valueBoxOutput(outputId = 'paExpected',
                              width = NULL)
             )
           )
    )
  )
)
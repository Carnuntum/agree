
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
                       p('On the right side you can \
                         upload your 2 by 2 contingency data \
                         via a .csv file.'),
                       p('The file has to have the \
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
                         width = NULL)))
                        
                        )
               )
)
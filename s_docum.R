#source('text.R')

documentation <- tabItem(tabName = 'docum',
                         fluidRow(
                           column(
                             width = 10,
                             offset = 1,
                             style = 'text-align: center;',
                             titlePanel(
                               h2("Wizagree - let the shiny magic do the work!",
                                  style = 'text-align: center; font-family: calibri;')
                               ),
                             box(
                               title = docum_text,
                               #imageOutput('timelineImage'),
                               width = NULL)
                           )
                         )
                         
                         
                         )

makeDecBox <- function(groupID, charNames, charVals, orientation = 'horizontal', individual = F, ...) {
  
  args <- list(...)
  
  bttns <- div(class = 'checkbox_button_css',
    checkboxGroupButtons(inputId = groupID,
                         choiceNames = charNames,
                         choiceValues = charVals,
                         direction = orientation,
                         individual = individual)
    )
  
  outObj <- fluidRow(column(
    width = if(groupID == 'scaleGroup') {10} else if(groupID == 'raterGroup') {8} else {6},
    offset = if(groupID == 'scaleGroup') {1} else if(groupID == 'raterGroup') {2} else {3},
    style = 'padding-left: 0px;',
    
    if(groupID %in% c('nomTwo', 'nomMore',
                      'ordTwo', 'ordMore',
                      'interTwo', 'interMore',
                      'ratioTwo', 'ratioMore')) {
      dropMenu(
        div(id = paste0(groupID, 'Drop'),
            box(
              width = NULL,
              bttns,
              style = centerText
            )),
        HTML(kableExtra::kable(t(eval(str2expression(paste0(groupID, 'Desc'))))) %>% 
               kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri')),
        trigger = 'mouseenter',
        theme = 'translucent',
        placement = 'bottom',
        maxWidth = '70em'
      )
    } else {
      box(
        width = NULL,
        bttns,
        args,
        style = centerText
      )
    }
  ))
  
  return(outObj)
}

resetDecTree <- function(session, divList, boxList) {
  
  updateCheckboxGroupButtons(session, inputId = 'scaleGroup', selected = 'NULL')
  
  for(i in 1:length(divList)) {
    shinyjs::hide(id = divList[i])
  }
  
  for(i in 1:length(boxList)) {
    updateCheckboxGroupButtons(session, inputId = boxList[i], selected = 'NULL')
  }
}



decisionTree <- tabItem(tabName = 'decTree',
                        
                        div(id = 'scaleID',
                          makeDecBox('scaleGroup', c('nominal','ordinal','interval','ratio'),
                                     c('nominal','ordinal','interval','ratio'),
                                     individual = T,
                                     ... = actionButton(inputId = 'reset', 'start over')),
                          ),
                        
                        shinyjs::hidden(
                            div(id = 'nRaters',
                                makeDecBox('raterGroup', c('2 raters', 'more than 2 raters'),
                                           c('two', 'more'),
                                           individual = T))
                        ),
                        
                        
                        shinyjs::hidden(
                          div(id = 'nominal two',
                              makeDecBox('nomTwo', nomTwoNames, nomTwoVals,
                                         orientation = 'vertical',
                                         individual = F
                              )
                          )
                        ),
                        
                        shinyjs::hidden(
                          div(id = 'nominal more',
                              makeDecBox('nomMore', nomMoreNames, nomMoreVals,
                                         orientation = 'vertical',
                                         individual = F
                                         ))
                        ),
                        
                        shinyjs::hidden(
                          div(id = 'ordinal two',
                              makeDecBox('ordTwo', ordTwoNames, ordTwoVals,
                                         orientation = 'vertical',
                                         individual = F
                              ))
                        ),
                        
                        shinyjs::hidden(
                          div(id = 'ordinal more',
                              makeDecBox('ordMore', ordMoreNames, ordMoreVals,
                                         orientation = 'vertical',
                                         individual = F
                              ))
                        ),
                        
                        shinyjs::hidden(
                          div(id = 'interval two',
                              makeDecBox('interTwo', interTwoNames, interTwoVals,
                                         orientation = 'vertical',
                                         individual = F
                              ))
                        ),
                        
                        shinyjs::hidden(
                          div(id = 'interval more',
                              makeDecBox('interMore', interMoreNames, interMoreVals,
                                         orientation = 'vertical',
                                         individual = F
                              ))
                        ),
                        
                        shinyjs::hidden(
                          div(id = 'ratio two',
                              makeDecBox('ratioTwo', ratioTwoNames, ratioTwoVals,
                                         orientation = 'vertical',
                                         individual = F
                              ))
                        ),
                        
                        shinyjs::hidden(
                          div(id = 'ratio more',
                              makeDecBox('ratioMore', ratioMoreNames, ratioMoreVals,
                                         orientation = 'vertical',
                                         individual = F
                              ))
                        ),
                        
                        fluidRow(
                          column(
                            width = 10,
                            offset = 1,
                            style = 'padding-left: 0px;',
                            box(
                              id = 'decTreeDocum',
                              width = NULL,
                              style = centerText,
                              title = decTree_text,
                              fileInput(inputId = 'histInput',
                                        label = 'Browse for .csv files',
                                        accept = '.csv'),
                              actionButton(inputId = 'randomHist',
                                           label = 'Generate example'),
                              hidden(
                                div(id = 'hiddenHist',
                                    verticalLayout(
                                      plotOutput("rainBow"),
                                      plotOutput("gridSplit"),
                                      br(),
                                      downloadButton(outputId = 'plotHistDown',
                                                     label = 'Download')
                                    )
                                )
                              ),
                              br()
                            ))
                        )
                        
                        
                        
                        )


refs <- tabItem(tabName = 'refs',
                  fluidRow(
                    box(title = 'References',
                        width = NULL,
                        tags$ul(
                          p(HTML(paste0('<li>',
                                        refList,
                                        '</li>',
                                        br())
                          )
                          )
                        ))
                  )
                )

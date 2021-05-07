#source('text.R')

documentation <- tabItem(tabName = 'docum',
                         fluidRow(
                           column(
                             width = 10,
                             offset = 1,
                             style = 'text-align: center;',
                             titlePanel('Agreement Measures Calculator'),
                             fluidRow(class = 'homeDocumBtns',
                               fluidRow(actionButton(inputId = 'imp_text',
                                            label = 'read me first',
                                            style = 'font-size: 35px; margin: 10px;'),
                                        hidden(
                                          div(id = 'test1',
                                              fluidRow(class = 'documRow',
                                                       column(width = 10,
                                                              offset = 1,
                                                              box(title = docum_text,
                                                                  width = NULL)
                                                       )
                                              )
                                          )
                                        )),
                               fluidRow(actionButton(inputId = 'imp_text2',
                                            label = 'read me next',
                                            style = 'font-size: 35px; margin: 10px;'),
                                        hidden(
                                          div(id = 'test2',
                                              fluidRow(class = 'documRow',
                                                       column(width = 10,
                                                              offset = 1,
                                                              box(title = docum_text,
                                                                  width = NULL)
                                                       )
                                              )
                                          )
                                        )),
                               fluidRow(actionButton(inputId = 'imp_text3',
                                            label = 'now read me!',
                                            style = 'font-size: 35px; margin: 10px;'),
                                        hidden(
                                          div(id = 'test3',
                                              fluidRow(class = 'documRow',
                                                       column(width = 10,
                                                              offset = 1,
                                                              box(title = docum_text,
                                                                  width = NULL)
                                                       )
                                              )
                                          )
                                        ))
                             )
                           )
                         )
                         
                         
                         )

makeDecBox <- function(groupID, charNames, charVals, orientation = 'horizontal', individual = F) {
  
  bttns <- div(class = 'checkbox_button_css',
    checkboxGroupButtons(inputId = groupID,
                         choiceNames = charNames,
                         choiceValues = charVals,
                         direction = orientation,
                         individual = individual)
    )
  
  outObj <- fluidRow(column(
    width = 10, offset = 1,
    style = 'padding-left: 0px; padding-right: -5px;',
    box(
      width = NULL,
      bttns,
      style = centerText
    )
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
                        fluidRow(
                          column(
                            width = 10,
                            offset = 1,
                            style = 'padding-left: 0px; padding-right: -5px;',
                            box(
                              id = 'decTreeDocum',
                              width = NULL,
                              style = 'text-align:center; ',
                              h3("Deciding Which Measure To Use"),
                              actionButton(inputId = 'reset', 'start over')
                            ))
                        ),
                        div(id = 'scaleID',
                          makeDecBox('scaleGroup', c('nominal','ordinal','interval','ratio'),
                                     c('nominal','ordinal','interval','ratio'),
                                     individual = T)),
                        
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
                                         ))
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
                        
                        
                        
                        
                        )
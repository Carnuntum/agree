#-------------------------------------------------------------------------------
#'*------------------------- ERROR MESSAGES FOR USER --------------------------*
#-------------------------------------------------------------------------------
kableError <- data.frame(
  "invalid data" = "check your data carefully"
)

warning_handler <- function(expr) {
  msg <<- list()
  
  withCallingHandlers(expr,
                      warning = function(w) {
                        msg <<- append(msg, conditionMessage(w))
                        print(paste('warning_handler warning: ', msg))
                        invokeRestart("muffleWarning")
                      },
                      error = function(e) {
                        msg <<- append(msg, conditionMessage(e))
                        print(paste('warning_handler error: ', msg))
                      })
  if(length(msg) == 0) {
    msg <<- NULL
  }
  return(expr)
}


btnPressWithoutData <- function() {
  showModal(
    div(class = 'modaldiag',
        modalDialog(
        title = 'Ups.. something went wrong!',
        'You pressed run without supplying any data!',
        easyClose = T,
        style = 'text-align: center;'
      )
    )
  )
}

tooManyIcc <- function(output) {
  showModal(
    div(class = 'modaldiag',
        modalDialog(
          title = 'Ups.. something went wrong!',
          'Please select exaclty 2 options (oneway or twoway and single or average) for the ICC calculation!',
          easyClose = T,
          style = 'text-align: center;'
        )
    )
  )
  iccErrOut(output)
  
}

tooManyOmega <- function() {
  showModal(
    div(class = 'modaldiag',
        modalDialog(
          title = 'Ups.. something went wrong!',
          'Please Select Exaclty 3 Options For The Calculation Of Sklars Omega!',
          easyClose = T,
          style = 'text-align: center;'
        )
    )
  )
}

tooManyKripp <- function() {
  showModal(
    div(class = 'modaldiag',
        modalDialog(
          title = 'Ups.. something went wrong!',
          'Please Select Exaclty 1 Option For The Scale Level!',
          easyClose = T,
          style = 'text-align: center;'
        )
    )
  )
}

# pa_wrongCatNumber <- function() {
#   showModal(
#     modalDialog(
#       title = 'Something went wrong!',
#       'The categories integer you entered seems to be wrong!',
#       easyClose = T,
#       style = 'text-align: center;'
#     )
#   )
# }

spear_no_exact_p <- function() {
  showModal(
    div(class = 'modaldiag',
        modalDialog(
                  title = 'Warning!',
                  msg,
                  easyClose = T,
                  style = 'text-align: center;'
      )
    )
  )
}

oddsErrOut <- function(output) {
  output$odds1 <- renderValueBox({
    valueBox(
      value = h4("Invalid Data",
                 style = 'text-align: center;
                    padding-top: 10px;
                    font-size: 25px;'),
      subtitle = ''
    )
  })
}

polycWarn <- function() {
  out <- modal_warn_out(polycGlobalWarning)
  
  showModal(
    div(class = 'modaldiag',
        modalDialog(
          title = 'Warning!',
          div(out),
          easyClose = T,
          style = 'text-align: center;'
      )
    )
  )
}

ordErrOut <- function(output) {
  output$ord1 <- renderValueBox({
    valueBox(
      value = h4("Invalid Data",
                 style = 'text-align: center;
                    padding-top: 10px;
                    font-size: 25px;'),
      subtitle = ''
    )
  })
}

polycErrOut <- function(output, test = 1) {
  output$polyc1 <- renderValueBox({
    valueBox(
      value = h4("Invalid Data",
                 style = 'text-align: center;
                    padding-top: 10px;
                    font-size: 25px;'),
      subtitle = test$warn
    )
  })
}

iccErrOut <- function(output) {
  output$icc1 <- renderValueBox({
    valueBox(
      value = h4("Error!",
                 style = 'text-align: center;
                         padding-top: 10px;
                         font-size: 25px;'),
      subtitle = ''
    )
  })
}

omegaErrOut <- function(output) {
  output$omega <- renderValueBox({
    tryCatch({
      input$omegaTest1
      valueBox(
        value = h4("An Error Occured",
                   style = 'text-align: center;
                         padding-top: 10px;
                         font-size: 25px;'),
        subtitle = p(HTML(paste0('Please Look Carefully At Your Data 
                               And Your Choices for Calculation',
                                 br(),
                                 if(length(omegaMessages) == 1) {
                                   circleButton(inputId = 'moreOmegaInfo',
                                                icon = icon('exclamation'),
                                                size = 'sm')
                                 })
                          ),
                     style = 'text-align: center; padding-top: 10px;')
        
      )
    }, error = function(e) {
      print(e)
    }, warning = function(w) {
      print(w)
    })
  })
}

moreOmegaInfo <- function() {
  showModal(
    div(class = 'modaldiag',
        modalDialog(
          title = 'Messages',
          omegaMessages,
          easyClose = T,
          style = 'text-align: center;'
        )
    )
  )
}


modal_warn_out <- function(warn_msg) {
  l <- list()
  out <- list()
  for(i in seq_along(warn_msg)) {
    l[i] <- paste0(i, ": {", warn_msg[i], "}")
  }
  
  for(i in seq_along(l)) {
    if(i %% 2 != 0) {
      out <- append(out, l[i])
    }
    if(i %% 2 == 0) {
      out <- rlist::list.append(out, br())
      out <- rlist::list.append(out, br())
      out <- rlist::list.append(out, l[i])
      out <- rlist::list.append(out, br())
      out <- rlist::list.append(out, br())
    }
  }
  
  showModal(
    div(class = 'modaldiag',
        modalDialog(
          title = 'Warning!',
          div(out),
          easyClose = T,
          style = centerText
        )
    )
  )
}

invalid_data <- function(output, out_id) {
  output[[out_id]] <- renderValueBox({
    valueBox(
      value = p(HTML(paste0(
        h4("Ups..something wrong happened!",
        style = 'text-align: center;
                padding-top: 10px;
                font-size: 25px;')
        ))),
      subtitle = div(class = 'warningButtonClass',
        p(HTML(paste(
        circleButton(inputId = 'warningButton',
                     icon = icon("exclamation"),
                     size = 's')
      ))), style = 'text-align:center;')
    )
  })
}

histPlotError <- function() {
  rainbow <- 
    ggplot() + geom_emoji("thinking", color='#3c8dbc') +
    xlab('Some error happened! Probably incompatible data?') +
    theme(panel.background = element_rect(fill = histPlotCol),
          plot.background = element_rect(fill = histPlotCol, colour = histPlotCol),
          axis.text = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(colour = '#3c8dbc', size = 20),
          panel.grid = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())
  
  grid <- 
    ggplot() + geom_emoji("confused", color='#3c8dbc') +
    xlab('Some error happened! Probably incompatible data?') +
    theme(panel.background = element_rect(fill = histPlotCol),
          plot.background = element_rect(fill = histPlotCol, colour = histPlotCol),
          axis.text = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(colour = '#3c8dbc', size = 20),
          panel.grid = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())
  
  return(list(rainbow, grid))
}





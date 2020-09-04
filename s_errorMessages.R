#-------------------------------------------------------------------------------
#'*------------------------- ERROR MESSAGES FOR USER --------------------------*
#-------------------------------------------------------------------------------
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

pa_wrongCatNumber <- function() {
  showModal(
    modalDialog(
      title = 'Something went wrong!',
      'The categories integer you entered seems to be wrong!',
      easyClose = T,
      style = 'text-align: center;'
    )
  )
}

spear_no_exact_p <- function() {
  showModal(
    div(class = 'modaldiag',
        modalDialog(
                  title = 'Warning!',
                  'Exact p-value cannot be calculated with ties in dataset!',
                  easyClose = T,
                  style = 'text-align: center;'
      )
    )
  )
}

polycWarn <- function() {
  showModal(
    div(class = 'modaldiag',
        modalDialog(
          title = 'Warning!',
          warn,
          easyClose = T,
          style = 'text-align: center;'
      )
    )
  )
}

#-------------------------------------------------------------------------------
#have to source all tabs which are used in the tabItems row, so each tab in the
#sidebar has to be sourced here
src_body <- list.files(pattern = '^s_')
lapply(src_body, source)

source('style.R')


body <- dashboardBody(
  includeCSS('style.css'),
  
  
  #site color variables -> find in style.R
  navbarCol,
  bodyCol,
  boxCol,
  tabCol,
  
  # bttnCol <- tags$head(tags$style(HTML(
  #   '.btn {transition-duration:0.4s}
  #   .btn:hover {background-color:grey; 
  #               color:white}
  #   .btn {border: none;}'
  # ))),
  
  
  #side <- tags$style(HTML('.skin-blue .main-sidebar .sidebar {background-color: darkcyan;}')),
  
  #changes in UI have to be registered here with the uiOutput function
  uiOutput('color'),
  uiOutput('bttnCol'),
  
  
  #'*---------------------------------------------------------------------------
  #'*-- PUT HERE ALL TABITEM VARIABLE NAMES HERE REGARDLESS IF SUBITEM OR NOT -*
  #'----------------------------------------------------------------------------
  tabItems(
    documentation,
    chi,
    percentAgree,
    percAgrPN,    # percentAgree is a subitem within the "all Methods" tabitem 
    cohenk,       # but the posNegPA is a subitem within the subitem and does
    ordinalRank  # not belong here
  )               
)

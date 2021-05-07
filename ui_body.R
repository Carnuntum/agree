
#-------------------------------------------------------------------------------
#have to source all tabs which are used in the tabItems row, so each tab in the
#sidebar has to be sourced here

src_body <- list.files(pattern = '^s_')
lapply(src_body, source)


body <- dashboardBody(
  includeCSS('style.css'),
  
  # fixedPanel(
  #   style = "z-index: 100;",
  #   dropdownButton(
  #     inputId = 'settings',
  #     label = 'settings',
  #     width = '160px',
  #     circle = F,
  #     actionButton(
  #       inputId = 'darkmode',
  #       label = 'darkmode',
  #       width = '140px',
  #       style = 'height: 30px;font-size: 15px;'
  #     ),
  #     
  #     actionButton(
  #       inputId = 'changCol',
  #       label = 'change hover',
  #       width = '140px',
  #       style = 'margin-top: 10px;height: 30px;font-size: 15px;'
  #     ),
  #     
  #     #NAVIGATION BACK TO HOME TAB
  #     actionButton(
  #       inputId = 'home',
  #       label = 'home',
  #       width = '140px',
  #       style = 'margin-top: 10px;height: 30px;font-size: 15px;'
  #     ),
  #     right = T,
  #     status = 'settBtn'
  #     
  #   ),
  #   top = "5%",
  #   bottom = "90%",
  #   left = "90%",
  #   right = "5%"
  # ),
  
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
  uiOutput('dark'),
  uiOutput('bttnCol'),
  
  
  #'*---------------------------------------------------------------------------
  #'*-- PUT HERE ALL TABITEM VARIABLE NAMES REGARDLESS IF SUBITEM OR NOT ------*
  #'----------------------------------------------------------------------------
  tabItems(
    documentation,
    decisionTree,
    kappa_pi,
    kappa_cohen,
    other_randIndex,       
    kappa_conger,
    kappa_fleiss,
    kappa_brennan,
    other_rwg,
    other_awg,
    other_bangdiwala,
    kappa_aickin,
    kappa_byrt,
    other_iota,
    kappa_gwet,
    other_entropy,
    kappa_eye,
    other_ad,
    kappa_akappa,
    kappa_free,
    other_infoAgree,
    icc,
    omega,
    krippendorf,
    other_occc
  )               
)

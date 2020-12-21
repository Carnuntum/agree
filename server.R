server <- function(input, output, session) {
  
  source('exampleData.R')
  source('style.R')
  
  for (f in dir(pattern="^s_")) {
    source(f, local = T)
  }
  useShinyjs()
  #-------------------------------------------------------------------------------
  
  #docum tabitem -> onclick shinyjs is used to activate an expression or function
  #when an html element is clicked -> also works with text links etc.
  #-> find in docum file -> whole html element is hidden at first then will
  #switch to visible when one of the buttons is pressed and vice versa
  lapply(list('imp_text', 'imp_text2', 'imp_text3'), onclick,
         toggle(id = 'test', anim = T))
  
  shinyjs::onclick(id = 'krippDocum', shinyjs::toggle(id = 'krippDocumBox',
                                                      anim = T))
  
  observe({
    req(input$ordinalInput)
    session$sendCustomMessage("upload_msg", "valid file")
  })
  
  
  
  #-----------------------------------------------------------------------------
  #'*-------------------------- NAVIGATION LOGIC ------------------------------*
  #-----------------------------------------------------------------------------
  observeEvent(input$home, {
    updateTabItems(session, 'dbSidebar', 'docum') #change page to docum tab when
                                                  #clicking home button top-right
                                                  #use the 'tabName' from tab
                                                  #which should be changed to
  })
  
  ExampleTables(input, output)
  
  defaultOutAll(input, output)
  #-----------------------------------------------------------------------------
  #'*--------------------------------- CHI ------------------------------------*
  #-----------------------------------------------------------------------------
  #chiExampleTable(input, output) #function for chi table example data to be 
                                 #displayed
  
  
  chiData <- reactive({ #save browsed user input for data
    read.csv(input$chiInput$datapath)
  })
  
  #run button for chi calculation for example table data
  observeEvent(input$test_chi, {
    set_tabset(output, 'ui_chi', id = 'id_chi', tableId = 'tab_chi', btnInputId = 'test_chi',
               downId = 'down_chi', data = xmp_chi)
    chiOut(input, output, xmp_chi)
  })
  
  
  #real calculation from user input data 
  observeEvent(input$chiInput, {
    
    set_tabset(output, 'ui_chi', id = 'id_chi', tableId = 'tab_chi', btnInputId = 'test_chi',
               downId = 'down_chi', data = chiData())
    
    })
  
  observeEvent(input$chiRun, {
    chiOut(input, output, chiData())
  })
  
  output$down_chi <- downloadHandler(
    filename = paste0('example_chi_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_chi, file, row.names = T)
    })
  
  output$chiFullDown <- downloadHandler(
    filename = paste0('results_chi_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_chi, file, row.names = T)
    })
  
  #-----------------------------------------------------------------------------
  #'*--------------------------------- ODDS -----------------------------------*
  #-----------------------------------------------------------------------------
  oddsData <- reactive({
    read.csv(input$oddsInput$datapath)
    
  })
  
  observeEvent(input$test_odds, {
    set_tabset(output, out_id = 'ui_odds', id = 'id_odds', tableId = 'tab_odds', downId = 'down_odds',
               btnInputId = 'test_odds', data = xmp_odds)
    oddsMainOut(input, output, xmp_odds)
    })
  
  observeEvent(input$oddsInput, {
    #show_uploaded(output, 'tab_odds', oddsData())
    set_tabset(output, out_id = 'ui_odds', id = 'id_odds', tableId = 'tab_odds', downId = 'down_odds',
               btnInputId = 'test_odds', data = oddsData())
  })
  
  observeEvent(input$oddsRun, {
    if(is.null(input$oddsInput)) {
      btnPressWithoutData()
    } else {
      oddsMainOut(input, output, oddsData())
    }
  })
  
  output$down_odds <- downloadHandler(
    filename = paste0('example_odds_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_odds, file, row.names = T)
    })
  
  
  output$oddsFullDown <- downloadHandler(
    filename = paste0('results_odds_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_odds, file, row.names = T)
    })
  
  #-----------------------------------------------------------------------------
  #'*------------------------ PERCENT AGREEMENT -------------------------------*
  #-----------------------------------------------------------------------------
  #paExampleTable(input, output) #function for example data in s_percAgree.R
  #paExampleTablePN(input, output)
  #paOutDefault(input, output) #default output for valueboxes
  
  paData <- eval(paData, envir = environment())
  
  pnData <- eval(pnData, envir = environment())
  
  #run button for example data
  observeEvent(input$test_pa, {
    set_tabset(output, 'ui_pa', id = 'id_pn', tableId = 'tab_pa', btnInputId = 'test_pa',
               downId = 'down_pa', data = xmp_pa)
    paOut(input, output, data = xmp_pa)
  })
  
  observeEvent(input$test_pn, {
    set_tabset(output, 'ui_pn', id = 'id_pn', tableId = 'tab_pn',
               btnInputId = 'test_pn', downId = 'down_pn', data = xmp_pn)
    pnOut(input, output, data = xmp_pn)
  })
  
  observeEvent(input$paInput, {
    set_tabset(output, 'ui_pa', id = 'id_pa', 'Uploads', 'Your Data', tableId = 'tab_pa',
               btnInputId = 'test_pa', downId = 'down_pa', data = paData())
    
  })
  
  observeEvent(input$pnInput, {
    #show_uploaded(output, 'tab_pn', pnData())
    set_tabset(output, 'ui_pn', id = 'id_pn', 'Uploads', 'Your Data', tableId = 'tab_pn',
               btnInputId = 'test_pn', downId = 'down_pn', data = pnData())
  })
  
  #if run is pressed without userdata warning popup will be displayed
  #isolate hinders that anything happens when button is pressed without any
  #data uploaded.
  
  observeEvent(input$paRun,
               {
                 if (is.null(input$paInput)) {
                   btnPressWithoutData()
                 } else {
                   paOut(input, output, paData())
                 }
               })
  
  observeEvent(input$pnRun,
               {
                 if (is.null(input$pnInput)) {
                   btnPressWithoutData()
                 } else {
                   pnOut(input, output, pnData())
                 }
               })
  
  output$down_pa <- downloadHandler(
    filename = paste0('example_pa_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_pa, file, row.names = T)
    })
  
  
  output$paFullDown <- downloadHandler(
    filename = paste0('results_pa_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_pa, file, row.names = T)
    })
  
  output$down_pn <- downloadHandler(
    filename = paste0('example_pn_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_pn, file, row.names = T)
    })
  
  
  output$pnFullDown <- downloadHandler(
    filename = paste0('results_pn_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_pn, file, row.names = T)
    })
  
  
  #-----------------------------------------------------------------------------
  #'*------------------------------- KAPPA ------------------------------------*
  #-----------------------------------------------------------------------------
  #kappaOutDefault(input, output)
  #kappaExampleTable(input, output)
  
  kappaData <- reactive({ #user input data
    read.csv(input$kappaInput$datapath)
  })
  
  observeEvent(input$kappaInput, {
    set_tabset(output, 'ui_kappa', id = 'id_kappa', 'Uploads', 'Your Data', tableId = 'tab_kappa',
               btnInputId = 'test_kappa', downId = 'down_kappa', data = kappaData())
  })
  
  observeEvent(input$test_kappa, kappaOut(input, output, xmp_kappa, method = 'kappa'))
  # data <- reactive({
  #   if (is.null(input$input)) return(NULL)
  #   file <- input$input
  #   df <- as.data.frame(read_csv(file$datapath))
  #   df <- table(df)
  #   return(df)
  # })
  # 
  # #show users uploded data in table format
  # output$showData <- renderTable( data())
  observeEvent(input$kappa,
               {
                 if (is.null(input$kappaInput)) {
                   btnPressWithoutData()
                 } else {
                   kappaOut(input, output, kappaData(), 'kappa')
                 }
               })
  
  observeEvent(input$spi,
               {
                 if (is.null(input$kappaInput)) {
                   btnPressWithoutData()
                 } else {
                   kappaOut(input, output, kappaData(), 'spi')
                 }
               })
  
  observeEvent(input$fleissKappa,
               {
                 if (is.null(input$kappaInput)) {
                   btnPressWithoutData()
                 } else {
                   kappaOut(input, output, kappaData(), 'fleissKappa')
                 }
               })
  
  output$down_kappa <- downloadHandler(
    filename = paste0('example_kappa_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_kappa, file, row.names = T)
    })
  
  
  output$kappaFullDown <- downloadHandler(
    filename = paste0('results_kappa_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_kappa, file, row.names = T)
    })
  
  #-----------------------------------------------------------------------------
  #'*--------------------------- SPEARMAN RHO ---------------------------------*
  #-----------------------------------------------------------------------------
  #spearExp(input, output)
  #spearOutDefault(input, output)
  
  ordinalData <- reactive({ #user input data
    tryCatch({
      read.csv(input$ordinalInput$datapath)
    }, error = function(e) {
      print(e)
    }, warning = function(w) {
      print(w)
    })
  })
  
  observeEvent(input$ordinalInput, {
    set_tabset(output, out_id = 'ui_spear', id = 'id_spear',
               tabTitle = 'Uploads', boxTitle = 'Your Data',
               tableId = 'tab_spear', downId = 'down_spear',
               btnInputId = 'test_spear', data = ordinalData())
  })
  
  observeEvent(input$test_spear, {
    set_tabset(output, out_id = 'ui_spear', id = 'id_spear',
               tableId = 'tab_spear', downId = 'down_spear',
               btnInputId = 'test_spear', data = xmp_spear)
    ordinalRankOut(input, output, xmp_spear,'spearman')
  })
  
  observeEvent(input$spear_p_value, spear_no_exact_p())
  
  # spear_react_btn <- eventReactive(input$spearRank,
  #                               {T})
  # 
  # kendallW_react_btn <- eventReactive(input$kendRank,
  #                                    {T})
  
  observeEvent(input$spearRank,
               {
                 if (is.null(input$ordinalInput)) {
                   btnPressWithoutData()
                 } else {
                   ordinalRankOut(input, output, ordinalData(), 'spearman')
                 }
               })
  
  observeEvent(input$kendW,
               {
                 if (is.null(input$ordinalInput)) {
                   btnPressWithoutData()
                 } else {
                   ordinalRankOut(input, output, ordinalData(), 'kendW')
                 }
               })
  
  observeEvent(input$tauB,
               {
                 if(is.null((input$ordinalInput))) {
                   btnPressWithoutData()
                 } else {
                   ordinalRankOut(input, output, ordinalData(), 'tauB')
                 }
               })
  
  observeEvent(input$tauC,
               {
                 if(is.null((input$ordinalInput))) {
                   btnPressWithoutData()
                 } else {
                   ordinalRankOut(input, output, ordinalData(), 'tauC')
                 }
               })
  
  observeEvent(input$tauInt,
               {
                 if(is.null((input$ordinalInput))) {
                   btnPressWithoutData()
                 } else {
                   ordinalRankOut(input, output, ordinalData(), 'tauIntra')
                 }
               })
  
  output$down_spear <- downloadHandler(
    filename = paste0('example_data_spearman_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_spear, file, row.names = F)
  })
  
  output$rankFullDown <- downloadHandler(
    filename = paste0('results_rank_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_rank, file, row.names = T)
    })
  
  #-----------------------------------------------------------------------------
  #'*----------------------------- POLYCHORIC ---------------------------------*
  #-----------------------------------------------------------------------------
  polyAppendTab(session)
  
  polycData <- reactive({
      read.csv(input$polycInput$datapath)
  })
  
  observeEvent(input$polycInput, {
    set_tabset(output, 'ui_polyc1', id = 'id_polyc', 'Uploads', 'Your Data', tableId = 'tab_polyc1',
               btnInputId = 'test_polyc1', downId = 'down_polyc1', data = polycData())
  })
  
  observeEvent(input$test_polyc1, {
    set_tabset(output, 'ui_polyc1', id = 'id_polyc', tableId = 'tab_polyc1', btnInputId = 'test_polyc1',
               downId = 'down_polyc1', data = xmp_poly1)
    
    polyAppendTab(session)
    
    polycOut(input, output, xmp_poly1,'polychoric')
  })
  
  observeEvent(input$test_polyc2, {
    polycOut(input, output, xmp_poly2, 'polychoric')
  })
  
  #EXAMPLE DOWNLOAD ------------------------------------------------------------
  output$down_polyc1 <- downloadHandler(
    filename = paste0('example_data_polyc_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_poly1, file, row.names = F)
    })
  
  output$down_polyc2 <- downloadHandler(
    filename = paste0('example_data2_polyc_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_poly2, file, row.names = F)
    })
  
  
  output$polycFullDown <- downloadHandler(
    filename = paste0('results_polychor_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_polyc, file, row.names = T)
    })
  #EXAMPLE DOWNLOAD END --------------------------------------------------------
  
  observeEvent(input$polycWarn, {
    polycWarn()
  })
  
  observeEvent(input$polycRun,
               if(is.null(input$polycInput)) {
                 btnPressWithoutData()
               } else {
                 polycOut(input, output,
                          as.table(as.matrix(polycData())),
                          'polychoric')
               })
  
  observeEvent(input$kruskalG,
               if(is.null(input$polycInput)) {
                 btnPressWithoutData()
               } else {
                 polycOut(input, output, as.table(as.matrix(polycData())),
                          'kruskalG')
               })
  
  observeEvent(input$kruskalT,
               if(is.null(input$polycInput)) {
                 btnPressWithoutData()
               } else {
                 polycOut(input, output, as.table(as.matrix(polycData())),
                          'kruskalT')
               })
  
  
  #-----------------------------------------------------------------------------
  #'*-------------------------------- ICC -------------------------------------*
  #-----------------------------------------------------------------------------
  
  
  iccData <- reactive({
    read.csv(input$iccInput$datapath)
  })
  
  observeEvent(input$iccInput, {
    set_tabset(output, out_id = 'ui_icc', id = 'id_icc', tableId = 'tab_icc', downId = 'down_icc',
               btnInputId = 'test_icc', data = iccData())
  })
  
  observeEvent(input$test_icc, {
    iccMainOut(input, output, xmp_icc, test = T)
    set_tabset(output, out_id = 'ui_icc', id = 'id_icc', tableId = 'tab_icc', downId = 'down_icc',
               btnInputId = 'test_icc', data = xmp_icc)
  })
  
  observeEvent(input$iccRun,
               if(is.null(input$iccInput)) {
                 btnPressWithoutData()
               } else {
                 iccMainOut(input, output, iccData())
               })
  
  output$down_icc <- downloadHandler(
    filename = paste0('example_data_icc_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_icc, file, row.names = F)
    })
  
  output$iccFullDown <- downloadHandler(
    filename = paste0('results_icc_', Sys.Date(), '.xlsx'),
    contentType = '.xlsx',
    
    content = function(file) {
     openxlsx::write.xlsx(l_icc, file, row.names = F)
    })
  
  
  #-----------------------------------------------------------------------------
  #'*------------------------------ OMEGA -------------------------------------*
  #-----------------------------------------------------------------------------
  omegaData <- reactive({
    as.matrix(read.csv(input$omegaInput$datapath))
  })
  
  observeEvent(input$omegaInput, {
    set_tabset(output, out_id = 'ui_omega', id = 'id_omega',
               tabTitle = 'Uploads', boxTitle = 'Your Data', tableId = 'tab_omega', downId = 'down_omega',
               btnInputId = 'test_omega', data = omegaData())
  })
  
  shinyjs::hide(id = 'omegaSpinner')
  
  observeEvent(input$test_omega, {
    shinyjs::show('omegaSpinner')
    set_tabset(output, out_id = 'ui_omega', id = 'id_omega', tableId = 'tab_omega', downId = 'down_omega',
               btnInputId = 'test_omega', data = xmp_omega)
    
    omegaMainOut(input, output, xmp_omega)
  })
  
  observeEvent(input$omegaRun, {
    if(is.null(input$omegaInput)) {
      btnPressWithoutData()
    } else {
      shinyjs::show(id = 'omegaSpinner')
      omegaMainOut(input, output, omegaData())
    }
  })
  
  observeEvent(input$moreOmegaInfo, {
    moreOmegaInfo()
  })
  
  output$down_omega <- downloadHandler(
    filename = paste0('example_data_omega_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_omega, file, row.names = F)
    })
  
  output$omegaResDown <- downloadHandler(
    filename = paste0('results_sklar_omega_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_omega, file, row.names = T)
    })
  
  #-----------------------------------------------------------------------------
  #'*------------------------------ KRIPP -------------------------------------*
  #-----------------------------------------------------------------------------
  krippData <- reactive({
    as.matrix(read.csv(input$krippInput$datapath))
  })
  
  observeEvent(input$krippInput, {
    set_tabset(output, out_id = 'ui_kripp', id = 'id_kripp',
               tabTitle = 'Uploads', boxTitle = 'Your Data',
               tableId = 'tab_kripp', downId = 'down_kripp',
               btnInputId = 'test_kripp', data = krippData())
  })
  
  observeEvent(input$test_kripp, {
    if(length(input$krippChoice) != 1) {
      tooManyKripp()
    } else {
      set_tabset(output, out_id = 'ui_kripp', id = 'id_kripp',
                 tableId = 'tab_kripp', downId = 'down_kripp',
                 btnInputId = 'test_kripp', data = xmp_kripp)
      krippMainOut(input, output, xmp_kripp)
    }
  })
  
  observeEvent(input$krippRun, {
    if(length(input$krippChoice) != 1) {
      tooManyKripp()
    } 
    else if(is.null(input$krippInput)) {
      btnPressWithoutData()
    } else {
      krippMainOut(input, output, krippData())
    }
  })
  
  output$down_kripp <- downloadHandler(
    filename = paste0('example_data_krippendorff_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_kripp, file, row.names = F)
    })
  
  output$krippFullDown <- downloadHandler(
    filename = paste0('results_krippendorff_alpha_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_kripp, file, row.names = T)
    })
  
  
  #-----------------------------------------------------------------------------
  #'*-------------------------------- CCC -------------------------------------*
  #-----------------------------------------------------------------------------
  
  cccData <- reactive({
    as.matrix(read.csv(input$cccInput$datapath))
  })
  
  observeEvent(input$cccInput, {
    set_tabset(output, out_id = 'ui_ccc', id = 'id_ccc',
               tabTitle = 'Uploads', boxTitle = 'Your Data',
               tableId = 'tab_ccc', downId = 'down_ccc',
               btnInputId = 'test_ccc', data = cccData())
  })
  
  observeEvent(input$test_ccc, {
    if(length(input$cccChoice) != 1) {
      tooManyKripp()
    } else {
      
      cccTemp <- round(xmp_ccc[1:10,], 3)
      lapply(cccTemp, as.character)
      cccTemp[[1]][10] = '...'
      cccTemp[[2]][10] = '...'
      
      set_tabset(output, out_id = 'ui_ccc', id = 'id_ccc',
                 tableId = 'tab_ccc', downId = 'down_ccc',
                 btnInputId = 'test_ccc', data = cccTemp)
      
      cccMainOut(input, output, xmp_ccc)
    }
  })
  
  observeEvent(input$cccRun, {
    if(length(input$cccChoice) != 1) {
      tooManyKripp()
    } 
    else if(is.null(input$cccInput)) {
      btnPressWithoutData()
    } else {
      cccMainOut(input, output, cccData())
    }
  })
  
  output$down_ccc <- downloadHandler(
    filename = paste0('example_data_ccc_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_ccc, file)
    })
  
  output$cccFullDown <- downloadHandler(
    filename = paste0('results_ccc_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_ccc, file, row.names = T)
    })
  
  output$down_plot_ccc <- downloadHandler(
    filename = paste0('bland_altman_ccc_', Sys.Date(), '.png'),
    contentType = 'png',
    
    content = function(file) {
      ggplot2::ggsave(file, plot = blaltm)
    })
  
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  
  mainCol(input, output)
  
  btn_hover(input, output)
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  
  
  #-------------------------------------------------------------------------------   
  
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}
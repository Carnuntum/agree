server <- function(input, output, session) {
  
  # for (f in dir(pattern="^s_")) {
  #   print(paste0('sourcing: ', f))
  #   source(f, local = T)
  # }
  
  #-----------------------------------------------------------------------------
  
  #docum tabitem -> onclick shinyjs is used to activate an expression or function
  #when an html element is clicked -> also works with text links etc.
  #-> find in docum file -> whole html element is hidden at first then will
  #switch to visible when one of the buttons is pressed and vice versa
  # lapply(list('imp_text', 'imp_text2', 'imp_text3'), onclick,
  #        toggle(id = 'test', anim = T))
  
  shinyjs::onclick(id = 'imp_text', toggle(id = 'test1', anim = T))
  shinyjs::onclick(id = 'imp_text2', toggle(id = 'test2', anim = T))
  shinyjs::onclick(id = 'imp_text3', toggle(id = 'test3', anim = T))
  
  
  #-----------------------------------------------------------------------------
  #'*----------------- MEASURE DOCUMENTATION TOGGLE ---------------------------*
  #-----------------------------------------------------------------------------
  observeEvent(input$test, {
    print(input$test)
  })
  
  
  observeEvent(input$reset, {
    resetDecTree(session, resetDivList, resetBoxList)
  })
  
  observeEvent(input$scaleGroup, ignoreInit = T, {
    shinyjs::show('nRaters', anim = T, animType = 'fade')
  })
  
  observeEvent(input$raterGroup, ignoreInit = T, {
    #print(paste(input$scaleGroup, input$raterGroup, sep = " "))
    shinyjs::show(paste(input$scaleGroup, input$raterGroup, sep = " "), anim = T, animType = 'fade')
  })
  
  observeEvent(input$nomTwo, {
    updateTabItems(session, 'dbSidebar', input$nomTwo)
  })
  observeEvent(input$nomMore, {
    updateTabItems(session, 'dbSidebar', input$nomMore)
  })
  observeEvent(input$ordTwo, {
    updateTabItems(session, 'dbSidebar', input$ordTwo)
  })
  observeEvent(input$ordMore, {
    updateTabItems(session, 'dbSidebar', input$ordMore)
  })
  observeEvent(input$interTwo, {
    updateTabItems(session, 'dbSidebar', input$interTwo)
  })
  observeEvent(input$interMore, {
    updateTabItems(session, 'dbSidebar', input$interMore)
  })
  observeEvent(input$ratioTwo, {
    updateTabItems(session, 'dbSidebar', input$ratioTwo)
  })
  observeEvent(input$ratioMore, {
    updateTabItems(session, 'dbSidebar', input$ratioMore)
  })
  
  observeEvent(input$backToDecTree, {
    updateTabItems(session, 'dbSidebar', 'decTree')
  })
  
  
  
  #scotts pi
  measureDocumentationShow('piDocum', 'piDocumBox')
  #cohens kappa
  measureDocumentationShow('cohenDocum', 'cohenDocumBox')
  #adjusted rand index
  measureDocumentationShow('randIndexDocum', 'randIndexDocumBox')
  #congers kappa
  measureDocumentationShow('congerDocum', 'congerDocumBox')
  #fleiss kappa
  measureDocumentationShow('fleissDocum', 'fleissDocumBox')
  #brennan prediger kappa
  measureDocumentationShow('brennanDocum', 'brennanDocumBox')
  #rwg
  measureDocumentationShow('rwgDocum', 'rwgDocumBox')
  #awg
  measureDocumentationShow('awgDocum', 'awgDocumBox')
  #bangdiwala
  measureDocumentationShow('bangdiwalaDocum', 'bangdiwalaDocumBox')
  #aickin
  measureDocumentationShow('aickinDocum', 'aickinDocumBox')
  #byrt kappa
  measureDocumentationShow('byrtDocum', 'byrtDocumBox')
  #iota
  measureDocumentationShow('iotaDocum', 'iotaDocumBox')
  #gwet
  measureDocumentationShow('gwetDocum', 'gwetDocumBox')
  #entropy index
  measureDocumentationShow('entropyDocum', 'entropyDocumBox')
  #von eye
  measureDocumentationShow('eyeDocum', 'eyeDocumBox')
  #ad index
  measureDocumentationShow('adDocum', 'adDocumBox')
  #akappa
  measureDocumentationShow('akappaDocum', 'akappaDocumBox')
  #free kappa
  measureDocumentationShow('freeKappaDocum', 'freeKappaDocumBox')
  #information agreement
  measureDocumentationShow('infoAgreeDocum', 'infoAgreeDocumBox')
  
  #krippendorffs alpha
  measureDocumentationShow('krippDocum', 'krippDocumBox')
  
  
  #CUSTOM UPLOAD MESSAGE - PROBABLY NOT NEEDED
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
  #'*---------------------------- SCOTT PI ------------------------------------*
  #-----------------------------------------------------------------------------
  
  piData <- reactive({ #save browsed user input for data
    read.csv(input$piInput$datapath)
  })
  
  #run button for pi calculation for example table data
  observeEvent(input$test_pi, {
    set_tabset(output, 'ui_pi', id = 'id_pi', tableId = 'tab_pi', btnInputId = 'test_pi',
               downId = 'down_pi', data = xmp_spear)
    piOut(input, output, xmp_spear)
  })
  
  
  #real calculation from user input data 
  observeEvent(input$piInput, {
    
    set_tabset(output, 'ui_pi', id = 'id_pi', tableId = 'tab_pi', btnInputId = 'test_pi',
               downId = 'down_pi', data = piData())
    
    })
  
  observeEvent(input$piRun, {
    if(is.null(input$piInput)) {
      btnPressWithoutData()
    }
    else {
      piOut(input, output, piData())
    }
  })
  
  output$down_pi <- downloadHandler(
    filename = paste0('example_pi_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_spear, file, row.names = T)
    })
  
  output$piFullDown <- downloadHandler(
    filename = paste0('results_pi_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_pi, file, row.names = T)
    })
  
  
  
  #-----------------------------------------------------------------------------
  #'*---------------------------- COHEN KAPPA ---------------------------------*
  #-----------------------------------------------------------------------------
  #paExampleTable(input, output) #function for example data in s_percAgree.R
  #paExampleTablePN(input, output)
  #paOutDefault(input, output) #default output for valueboxes
  
  cohenData <- reactive({
    read.csv(input$cohenInput$datapath)
  })
  
  observeEvent(input$test_cohen, {
    set_tabset(output, 'ui_cohen', id = 'id_cohen', tableId = 'tab_cohen', btnInputId = 'test_cohen',
               downId = 'down_cohen', data = xmp_pa)
    cohenOut(input, output, data = xmp_pa)
  })
  
  observeEvent(input$cohenInput, {
    set_tabset(output, 'ui_cohen', id = 'id_cohen', 'Uploads', 'Your Data', tableId = 'tab_cohen',
               btnInputId = 'test_cohen', downId = 'down_cohen', data = cohenData())
    
  })
  
  observeEvent(input$cohenRun,
               {
                 if (is.null(input$cohenInput)) {
                   btnPressWithoutData()
                 } else {
                   cohenOut(input, output, cohenData())
                 }
               })
  
  output$down_cohen <- downloadHandler(
    filename = paste0('example_cohen_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_pa, file, row.names = T)
    })
  
  output$cohenFullDown <- downloadHandler(
    filename = paste0('results_cohen_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_cohen, file, row.names = T)
    })
  
  #-----------------------------------------------------------------------------
  #'*-------------------------- RAND INDEX ------------------------------------*
  #-----------------------------------------------------------------------------
  
  
  randIndexData <- reactive({
    read.csv(input$randIndexInput$datapath)
  })
  
  #run button for example data
  
  
  observeEvent(input$test_randIndex, {
    set_tabset(output, 'ui_randIndex', id = 'id_randIndex', tableId = 'tab_randIndex',
               btnInputId = 'test_randIndex', downId = 'down_randIndex', data = xmp_spear)
    randIndexOut(input, output, data = xmp_spear)
  })
  
  
  observeEvent(input$randIndexInput, {
    #show_uploaded(output, 'tab_pn', pnData())
    set_tabset(output, 'ui_randIndex', id = 'id_randIndex', 'Uploads', 'Your Data', tableId = 'tab_randIndex',
               btnInputId = 'test_randIndex', downId = 'down_randIndex', data = randIndexData())
  })
  
  #if run is pressed without userdata warning popup will be displayed
  #isolate hinders that anything happens when button is pressed without any
  #data uploaded.
  
  observeEvent(input$randIndexRun,
               {
                 if (is.null(input$randIndexInput)) {
                   btnPressWithoutData()
                 } else {
                   randIndexOut(input, output, randIndexData())
                 }
               })
  
  observeEvent(input$dbSidebar, {
    shiny::removeUI(selector = '#warningButton')
  })
  
  observeEvent(input$warningButton, {
    modal_warn_out(msg)
  })
  
  
  
  
  output$down_randIndex <- downloadHandler(
    filename = paste0('example_randIndex_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_spear, file, row.names = T)
    })
  
  
  output$randIndexFullDown <- downloadHandler(
    filename = paste0('results_randIndex_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_randIndex, file, row.names = T)
    })
  
  #-----------------------------------------------------------------------------
  #'*--------------------------- CONGER KAPPA ---------------------------------*
  #-----------------------------------------------------------------------------
  #spearExp(input, output)
  #spearOutDefault(input, output)
  
  congerData <- reactive({
    read.csv(input$congerInput$datapath)
  })
  
  observeEvent(input$congerInput, {
    set_tabset(output, out_id = 'ui_spear', id = 'id_conger',
               tabTitle = 'Uploads', boxTitle = 'Your Data',
               tableId = 'tab_conger', downId = 'down_conger',
               btnInputId = 'test_conger', data = congerData())
  })
  
  observeEvent(input$test_conger, {
    set_tabset(output, out_id = 'ui_conger', id = 'id_conger',
               tableId = 'tab_conger', downId = 'down_conger',
               btnInputId = 'test_conger', data = xmp_poly2)
    congerOut(input, output, xmp_poly2)
  })
  
  observeEvent(input$congerRun, {
    if(is.null(input$congerInput)) {
      btnPressWithoutData()
    } else {
      congerOut(input, output, congerData())
    }
  })
  
  output$down_conger <- downloadHandler(
    filename = paste0('example_data_conger_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_poly2, file, row.names = F)
    })
  
  output$congerFullDown <- downloadHandler(
    filename = paste0('results_conger_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_conger, file, row.names = T)
    })
  
  
  #-----------------------------------------------------------------------------
  #'*--------------------------- FLEISS KAPPA ---------------------------------*
  #-----------------------------------------------------------------------------
  fleissData <- reactive({
    read.csv(input$fleissInput$datapath)
    
  })
  
  observeEvent(input$test_fleiss, {
    set_tabset(output, out_id = 'ui_fleiss', id = 'id_fleiss', tableId = 'tab_fleiss', downId = 'down_fleiss',
               btnInputId = 'test_fleiss', data = xmp_poly2)
    fleissOut(input, output, xmp_poly2)
  })
  
  observeEvent(input$fleissInput, {
    set_tabset(output, out_id = 'ui_fleiss', id = 'id_fleiss', tableId = 'tab_fleiss', downId = 'down_fleiss',
               btnInputId = 'test_fleiss', data = fleissData())
  })
  
  observeEvent(input$fleissRun, {
    if(is.null(input$fleissInput)) {
      btnPressWithoutData()
    } else {
      fleissOut(input, output, fleissData())
    }
  })
  
  output$down_fleiss <- downloadHandler(
    filename = paste0('example_fleiss_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_poly2, file, row.names = T)
    })
  
  
  output$fleissFullDown <- downloadHandler(
    filename = paste0('results_fleiss_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_fleiss, file, row.names = T)
    })
  
  #-----------------------------------------------------------------------------
  #'*--------------------------------- RWG ------------------------------------*
  #-----------------------------------------------------------------------------
  
  rwgData <- reactive({ #user input data
    read.csv(input$rwgInput$datapath)
  })
  
  observeEvent(input$rwgInput, {
    set_tabset(output, 'ui_rwg', id = 'id_rwg', 'Uploads', 'Your Data', tableId = 'tab_rwg',
               btnInputId = 'test_rwg', downId = 'down_rwg', data = rwgData())
  })
  
  observeEvent(input$test_rwg, rwgOut(input, output, xmp_rwg))
  
  observeEvent(input$rwgRun,
               {
                 if (is.null(input$rwgInput)) {
                   btnPressWithoutData()
                 } else {
                   rwgOut(input, output, rwgData())
                 }
               })
  
  output$down_rwg <- downloadHandler(
    filename = paste0('example_rwg_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_rwg, file, row.names = T)
    })
  
  
  output$rwgFullDown <- downloadHandler(
    filename = paste0('results_rwg_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_rwg, file, row.names = T)
    })
  
  
  #-----------------------------------------------------------------------------
  #'*--------------------------------- AWG ------------------------------------*
  #-----------------------------------------------------------------------------
  
  awgData <- reactive({ #user input data
    read.csv(input$awgInput$datapath)
  })
  
  observeEvent(input$awgInput, {
    set_tabset(output, 'ui_awg', id = 'id_awg', 'Uploads', 'Your Data', tableId = 'tab_awg',
               btnInputId = 'test_awg', downId = 'down_awg', data = awgData())
  })
  
  observeEvent(input$test_awg, awgOut(input, output, xmp_rwg))
  
  observeEvent(input$awgRun,
               {
                 if (is.null(input$awgInput)) {
                   btnPressWithoutData()
                 } else {
                   awgOut(input, output, awgData())
                 }
               })
  
  output$down_awg <- downloadHandler(
    filename = paste0('example_awg_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_awg, file, row.names = T)
    })
  
  
  output$awgFullDown <- downloadHandler(
    filename = paste0('results_awg_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_awg, file, row.names = T)
    })
  
  
  
  #-----------------------------------------------------------------------------
  #'*---------------------- BRENNAN PREDIGER KAPPA ----------------------------*
  #-----------------------------------------------------------------------------
  # shinyjs::onclick(id = 'polytext', shinyjs::click('hiddenClick'))
  # observeEvent(input$hiddenClick, {
  #   if(input$hiddenClick == 1) {
  #     polyAppendTab(session)
  #   }
  # })
  
  brennanData <- reactive({
      read.csv(input$brennanInput$datapath)
  })
  
  observeEvent(input$brennanInput, {
    set_tabset(output, 'ui_brennan', id = 'id_brennan', 'Uploads', 'Your Data', tableId = 'tab_brennan',
               btnInputId = 'test_brennan', downId = 'down_brennan', data = brennanData())
  })
  
  observeEvent(input$test_brennan, {
    set_tabset(output, 'ui_brennan', id = 'id_brennan', tableId = 'tab_brennan', btnInputId = 'test_brennan',
               downId = 'down_brennan', data = xmp_poly2)
    
    brennanOut(input, output, xmp_poly2)
  })
  
  observeEvent(input$test_polyc2, {
    polycOut(input, output, xmp_poly2, 'polychoric')
  })
  
  #EXAMPLE DOWNLOAD ------------------------------------------------------------
  output$down_brennan <- downloadHandler(
    filename = paste0('example_data_brennan_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_poly2, file, row.names = F)
    })
  
  # output$down_polyc2 <- downloadHandler(
  #   filename = paste0('example_data2_polyc_', Sys.Date(), '.csv'),
  #   contentType = 'csv',
  #   
  #   content = function(file) {
  #     write.csv(xmp_poly2, file, row.names = F)
  #   })
  
  
  output$brennanFullDown <- downloadHandler(
    filename = paste0('results_brennan_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_brennan, file, row.names = T)
    })
  #EXAMPLE DOWNLOAD END --------------------------------------------------------
  
  
  observeEvent(input$brennanRun,
               if(is.null(input$brennanInput)) {
                 btnPressWithoutData()
               } else {
                 brennanOut(input, output, brennanData())
               })
  
  
  #-----------------------------------------------------------------------------
  #'*---------------------------- BANGDIWALA B --------------------------------*
  #-----------------------------------------------------------------------------
  
  bangdiwalaData <- reactive({ #user input data
    read.csv(input$bangdiwalaInput$datapath)
  })
  
  observeEvent(input$bangdiwalaInput, {
    set_tabset(output, 'ui_bangdiwala', id = 'id_bangdiwala', 'Uploads', 'Your Data', tableId = 'tab_bangdiwala',
               btnInputId = 'test_bangdiwala', downId = 'down_bangdiwala', data = bangdiwalaData())
  })
  
  observeEvent(input$test_bangdiwala, {
    set_tabset(output, 'ui_bangdiwala', id = 'id_bangdiwala', tableId = 'tab_bangdiwala', btnInputId = 'test_bangdiwala',
               downId = 'down_bangdiwala', data = xmp_kappa)
    
    bangdiwalaOut(input, output, as.data.frame(xmp_kappa))
    
    })
  
  observeEvent(input$bangdiwalaRun,
               {
                 if (is.null(input$bangdiwalaInput)) {
                   btnPressWithoutData()
                 } else {
                   bangdiwalaOut(input, output, bangdiwalaData())
                 }
               })
  
  output$down_bangdiwala <- downloadHandler(
    filename = paste0('example_bangdiwala_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(as.data.frame(xmp_kappa), file, row.names = T)
    })
  
  
  output$bangdiwalaFullDown <- downloadHandler(
    filename = paste0('results_bangdiwala_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_bangdiwala, file, row.names = T)
    })
  
  output$down_plot_bangdiwala <- downloadHandler(
    filename = paste0('agreement_plot_bangdiwala_', Sys.Date(), '.png'),
    contentType = 'png',
    
    content = function(file) {
      ggplot2::ggsave(file, plot = bangdiwala_plot)
    })
  
  
  #-----------------------------------------------------------------------------
  #'*--------------------------- AICKINS ALPHA --------------------------------*
  #-----------------------------------------------------------------------------
  
  aickinData <- reactive({ #user input data
    read.csv(input$aickinInput$datapath)
  })
  
  observeEvent(input$aickinInput, {
    set_tabset(output, 'ui_aickin', id = 'id_aickin', 'Uploads', 'Your Data', tableId = 'tab_aickin',
               btnInputId = 'test_aickin', downId = 'down_aickin', data = aickinData())
  })
  
  observeEvent(input$test_aickin, {
    set_tabset(output, 'ui_aickin', id = 'id_aickin', tableId = 'tab_aickin', btnInputId = 'test_aickin',
               downId = 'down_aickin', data = xmp_spear)
    
    aickinOut(input, output, xmp_spear)
    
    })
  
  observeEvent(input$aickinRun,
               {
                 if (is.null(input$aickinInput)) {
                   btnPressWithoutData()
                 } else {
                   aickinOut(input, output, aickinData())
                 }
               })
  
  output$down_aickin <- downloadHandler(
    filename = paste0('example_aickin_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(as.data.frame(xmp_spear), file, row.names = T)
    })
  
  
  output$aickinFullDown <- downloadHandler(
    filename = paste0('results_aickin_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_aickin, file, row.names = T)
    })
  
  
  #-----------------------------------------------------------------------------
  #'*----------------------------- BYRT KAPPA ---------------------------------*
  #-----------------------------------------------------------------------------
  
  byrtData <- reactive({ #user input data
    read.csv(input$byrtInput$datapath)
  })
  
  observeEvent(input$byrtInput, {
    set_tabset(output, 'ui_byrt', id = 'id_byrt', 'Uploads', 'Your Data', tableId = 'tab_byrt',
               btnInputId = 'test_byrt', downId = 'down_byrt', data = byrtData())
  })
  
  observeEvent(input$test_byrt, {
    set_tabset(output, 'ui_byrt', id = 'id_byrt', tableId = 'tab_byrt', btnInputId = 'test_byrt',
               downId = 'down_byrt', data = xmp_icc)
    
    byrtOut(input, output, xmp_icc)
    
    })
  
  observeEvent(input$byrtRun,
               {
                 if (is.null(input$byrtInput)) {
                   btnPressWithoutData()
                 } else {
                   byrtOut(input, output, byrtData())
                 }
               })
  
  output$down_byrt <- downloadHandler(
    filename = paste0('example_byrt_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(as.data.frame(xmp_icc), file, row.names = T)
    })
  
  
  output$byrtFullDown <- downloadHandler(
    filename = paste0('results_byrt_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_byrt, file, row.names = T)
    })
  
  
  #-----------------------------------------------------------------------------
  #'*------------------------------ IOTA --------------------------------------*
  #-----------------------------------------------------------------------------
  
  iotaData <- reactive({ #user input data
    read.csv(input$iotaInput$datapath)
  })
  
  observeEvent(input$iotaInput, {
    set_tabset(output, 'ui_iota', id = 'id_iota', 'Uploads', 'Your Data', tableId = 'tab_iota',
               btnInputId = 'test_iota', downId = 'down_iota', data = iotaData())
    
    shinyjs::reset(id = 'iotaNumInput')
  })
  
  observeEvent(input$test_iota, {
    
    iotaOut(input, output, xmp_icc, scope = F)
    
    })
  
  observeEvent(input$iotaRun,
               {
                 if (is.null(input$iotaInput)) {
                   btnPressWithoutData()
                 } else {
                   iotaOut(input, output, iotaData(), scope = T)
                 }
               })
  
  output$down_iota <- downloadHandler(
    filename = paste0('example_iota_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(as.data.frame(xmp_icc), file, row.names = T)
    })
  
  
  output$iotaFullDown <- downloadHandler(
    filename = paste0('results_iota_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_iota, file, row.names = T)
    })
  
  #-----------------------------------------------------------------------------
  #'*------------------------------ GWETS AC ----------------------------------*
  #-----------------------------------------------------------------------------
  
  gwetData <- reactive({ #user input data
    read.csv(input$gwetInput$datapath)
  })
  
  observeEvent(input$gwetInput, {
    set_tabset(output, 'ui_gwet', id = 'id_gwet', 'Uploads', 'Your Data', tableId = 'tab_gwet',
               btnInputId = 'test_gwet', downId = 'down_gwet', data = gwetData())
    
  })
  
  observeEvent(input$test_gwet, {
    set_tabset(output, 'ui_gwet', id = 'id_gwet', tableId = 'tab_gwet', btnInputId = 'test_gwet',
               downId = 'down_gwet', data = xmp_icc)
    
    gwetOut(input, output, xmp_icc)
    
  })
  
  observeEvent(input$gwetRun,
               {
                 if (is.null(input$gwetInput)) {
                   btnPressWithoutData()
                 } else {
                   gwetOut(input, output, gwetData())
                 }
               })
  
  output$down_gwet <- downloadHandler(
    filename = paste0('example_gwet_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(as.data.frame(xmp_icc), file, row.names = T)
    })
  
  
  output$gwetFullDown <- downloadHandler(
    filename = paste0('results_gwet_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_gwet, file, row.names = T)
    })
  
  #-----------------------------------------------------------------------------
  #'*---------------------------- ENTROPY INDEX -------------------------------*
  #-----------------------------------------------------------------------------
  
  entropyData <- reactive({ #user input data
    read.csv(input$entropyInput$datapath)
  })
  
  shinyjs::hide(id = 'entropySpinner')
  
  observeEvent(input$entropyInput, {
    set_tabset(output, 'ui_entropy', id = 'id_entropy', 'Uploads', 'Your Data', tableId = 'tab_entropy',
               btnInputId = 'test_entropy', downId = 'down_entropy', data = entropyData())
    
  })
  
  observeEvent(input$test_entropy, {
    shinyjs::show(id = 'entropySpinner')
    entropyOut(input, output, xmp_poly2, scope = F)
    
  })
  
  observeEvent(input$entropyRun,
               {
                 if (is.null(input$entropyInput)) {
                   btnPressWithoutData()
                 } else {
                   shinyjs::show('entropySpinner')
                   entropyOut(input, output, entropyData(), scope = T)
                 }
               })
  
  output$down_entropy <- downloadHandler(
    filename = paste0('example_entropy_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(as.data.frame(xmp_poly2), file, row.names = T)
    })
  
  
  output$entropyFullDown <- downloadHandler(
    filename = paste0('results_entropy_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_entropy, file, row.names = T)
    })
  
  
  #-----------------------------------------------------------------------------
  #'*---------------------------- VON EYE KAPPA -------------------------------*
  #-----------------------------------------------------------------------------
  
  eyeData <- reactive({ #user input data
    read.csv(input$eyeInput$datapath)
  })
  
  
  observeEvent(input$eyeInput, {
    set_tabset(output, 'ui_eye', id = 'id_eye', 'Uploads', 'Your Data', tableId = 'tab_eye',
               btnInputId = 'test_eye', downId = 'down_eye', data = eyeData())
    
  })
  
  observeEvent(input$test_eye, {
    eyeOut(input, output, xmp_poly2, scope = F)
    
  })
  
  observeEvent(input$eyeRun,
               {
                 if (is.null(input$eyeInput)) {
                   btnPressWithoutData()
                 } else {
                   eyeOut(input, output, eyeData(), scope = T)
                 }
               })
  
  output$down_eye <- downloadHandler(
    filename = paste0('example_eye_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(as.data.frame(xmp_poly2), file, row.names = T)
    })
  
  
  output$eyeFullDown <- downloadHandler(
    filename = paste0('results_eye_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_eye, file, row.names = T)
    })
  
  #-----------------------------------------------------------------------------
  #'*------------------------------ AD INDEX ----------------------------------*
  #-----------------------------------------------------------------------------
  
  adData <- reactive({ #user input data
    read.csv(input$adInput$datapath)
  })
  
  
  observeEvent(input$adInput, {
    set_tabset(output, 'ui_ad', id = 'id_ad', 'Uploads', 'Your Data', tableId = 'tab_ad',
               btnInputId = 'test_ad', downId = 'down_ad', data = adData())
    
  })
  
  observeEvent(input$test_ad, {
    set_tabset(output, 'ui_ad', id = 'id_ad', 'Uploads', 'Your Data', tableId = 'tab_ad',
               btnInputId = 'test_ad', downId = 'down_ad', data = xmp_icc)
    
    adOut(input, output, xmp_icc)
    
  })
  
  observeEvent(input$adRun,
               {
                 if (is.null(input$adInput)) {
                   btnPressWithoutData()
                 } else {
                   adOut(input, output, adData())
                 }
               })
  
  output$down_ad <- downloadHandler(
    filename = paste0('example_ad_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(as.data.frame(xmp_icc), file, row.names = T)
    })
  
  
  output$adFullDown <- downloadHandler(
    filename = paste0('results_ad_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_ad, file, row.names = T)
    })
  
  
  #-----------------------------------------------------------------------------
  #'*------------------------------- A KAPPA ----------------------------------*
  #-----------------------------------------------------------------------------
  
  akappaData <- reactive({ #user input data
    read.csv(input$akappaInput$datapath)
  })
  
  
  observeEvent(input$akappaInput, {
    set_tabset(output, 'ui_akappa', id = 'id_akappa', 'Uploads', 'Your Data', tableId = 'tab_akappa',
               btnInputId = 'test_akappa', downId = 'down_akappa', data = akappaData())
    
  })
  
  observeEvent(input$test_akappa, {
    set_tabset(output, 'ui_akappa', id = 'id_akappa', tableId = 'tab_akappa',
               btnInputId = 'test_akappa', downId = 'down_akappa', data = xmp_icc)
    
    akappaOut(input, output, xmp_icc)
    
  })
  
  observeEvent(input$akappaRun,
               {
                 if (is.null(input$akappaInput)) {
                   btnPressWithoutData()
                 } else {
                   akappaOut(input, output, akappaData())
                 }
               })
  
  output$down_akappa <- downloadHandler(
    filename = paste0('example_akappa_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(as.data.frame(xmp_icc), file, row.names = T)
    })
  
  
  output$akappaFullDown <- downloadHandler(
    filename = paste0('results_akappa_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_akappa, file, row.names = T)
    })
  
  #-----------------------------------------------------------------------------
  #'*----------------------------- FREE KAPPA ---------------------------------*
  #-----------------------------------------------------------------------------
  
  freeKappaData <- reactive({ #user input data
    read.csv(input$freeKappaInput$datapath)
  })
  
  
  observeEvent(input$freeKappaInput, {
    set_tabset(output, 'ui_freeKappa', id = 'id_freeKappa', 'Uploads', 'Your Data', tableId = 'tab_freeKappa',
               btnInputId = 'test_freeKappa', downId = 'down_freeKappa', data = freeKappaData())
    
  })
  
  observeEvent(input$test_freeKappa, {
    set_tabset(output, 'ui_freeKappa', id = 'id_freeKappa', tableId = 'tab_freeKappa',
               btnInputId = 'test_freeKappa', downId = 'down_freeKappa', data = xmp_pn)
    
    freeKappaOut(input, output, xmp_pn, scope = F)
    
  })
  
  observeEvent(input$freeKappaRun,
               {
                 if (is.null(input$freeKappaInput)) {
                   btnPressWithoutData()
                 } else {
                   freeKappaOut(input, output, freeKappaData(), scope = T)
                 }
               })
  
  output$down_freeKappa <- downloadHandler(
    filename = paste0('example_freeKappa_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(as.data.frame(xmp_pn), file, row.names = T)
    })
  
  
  output$freeKappaFullDown <- downloadHandler(
    filename = paste0('results_freeKappa_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_freeKappa, file, row.names = T)
    })
  
  #-----------------------------------------------------------------------------
  #'*----------------------- INFORMATION AGREEMENT ----------------------------*
  #-----------------------------------------------------------------------------
  
  infoAgreeData <- reactive({ #user input data
    read.csv(input$infoAgreeInput$datapath)
  })
  
  
  observeEvent(input$infoAgreeInput, {
    set_tabset(output, 'ui_infoAgree', id = 'id_infoAgree', 'Uploads', 'Your Data', tableId = 'tab_infoAgree',
               btnInputId = 'test_infoAgree', downId = 'down_infoAgree', data = infoAgreeData())
    
  })
  
  observeEvent(input$test_infoAgree, {
    set_tabset(output, 'ui_infoAgree', id = 'id_infoAgree', tableId = 'tab_infoAgree',
               btnInputId = 'test_infoAgree', downId = 'down_infoAgree', data = xmp_pn)
    
    infoAgreeOut(input, output, xmp_pn, scope = F)
    
  })
  
  observeEvent(input$infoAgreeRun,
               {
                 if (is.null(input$infoAgreeInput)) {
                   btnPressWithoutData()
                 } else {
                   infoAgreeOut(input, output, infoAgreeData(), scope = T)
                 }
               })
  
  output$down_infoAgree <- downloadHandler(
    filename = paste0('example_infoAgree_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(as.data.frame(xmp_pn), file, row.names = T)
    })
  
  
  output$infoAgreeFullDown <- downloadHandler(
    filename = paste0('results_infoAgree_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_infoAgree, file, row.names = T)
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
    # set_tabset(output, out_id = 'ui_omega', id = 'id_omega', tableId = 'tab_omega', downId = 'down_omega',
    #            btnInputId = 'test_omega', data = xmp_omega)
    
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
  #'*------------------------------- OCCC -------------------------------------*
  #-----------------------------------------------------------------------------
  
  occcData <- reactive({
    as.matrix(read.csv(input$occcInput$datapath))
  })
  
  observeEvent(input$occcInput, {
    set_tabset(output, out_id = 'ui_occc', id = 'id_occc',
               tabTitle = 'Uploads', boxTitle = 'Your Data',
               tableId = 'tab_occc', downId = 'down_occc',
               btnInputId = 'test_occc', data = occcData())
    
    shinyjs::hide(id = 'div_plot_occc') #hide bland altman when new data is uploaded
    
  })
  
  observeEvent(input$test_occc, {
      
      cccTemp <- round(xmp_ccc[1:10,], 3)
      lapply(cccTemp, as.character)
      cccTemp[[1]][10] = '...'
      cccTemp[[2]][10] = '...'
      
      occcOut(input, output, xmp_ccc, scope = F)
    
  })
  
  observeEvent(input$occcRun, {
    if(is.null(input$occcInput)) {
      btnPressWithoutData()
    } else {
      occcOut(input, output, occcData(), scope = T)
    }
  })
  
  output$down_occc <- downloadHandler(
    filename = paste0('example_data_occc_', Sys.Date(), '.csv'),
    contentType = 'csv',
    
    content = function(file) {
      write.csv(xmp_ccc, file)
    })
  
  output$occcFullDown <- downloadHandler(
    filename = paste0('results_occc_', Sys.Date(), '.xlsx'),
    contentType = 'xlsx',
    
    content = function(file) {
      openxlsx::write.xlsx(l_occc, file, row.names = T)
    })
  
  output$down_plot_occc <- downloadHandler(
    filename = paste0('bland_altman_occc_', Sys.Date(), '.png'),
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
  
  
  # if (!interactive()) {
  #   session$onSessionEnded(function() {
  #     stopApp()
  #     q("no")
  #   })
  # }
}
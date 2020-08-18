server <- function(input, output, session) {
  
  source('exampleData.R')
  source('style.R')
  
  for (f in dir(pattern="^s_")) {
    source(f, local = T)
  }
  #-------------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  #'*-------------------------- NAVIGATION LOGIC ------------------------------*
  #-----------------------------------------------------------------------------
  observeEvent(input$home, {
    updateTabItems(session, 'dbSidebar', 'docum') #change page to docum tab when
                                                  #clicking home button top-right
                                                  #use the 'tabName' from tab
                                                  #which should be changed to
  })
  
  
  #-----------------------------------------------------------------------------
  #'*--------------------- SERVER LOGIC FOR CHI INPUT -------------------------*
  #-----------------------------------------------------------------------------
  chiExampleTable(input, output) #function for chi table example data to be 
                                 #displayed
  
  
  chiData <- reactive({ #save browsed user input for data
    read_csv(input$chiInput$datapath)
  })
  
  #run button for chi calculation for example table data
  observeEvent(input$chiTest, chiOut(input, output, chiTableExp))
  
  #default output for the chi valueboxes
  chiOutDefault(input, output)
  
  #real calculation from user input data 
  observeEvent(input$chiInput, c(chiOut(input, output, data = chiData())))
  

  
  
  #-----------------------------------------------------------------------------
  #'*------------------------ PERCENT AGREEMENT -------------------------------*
  #-----------------------------------------------------------------------------
  paExampleTable(input, output) #function for example data in s_percAgree.R
  paExampleTablePN(input, output)
  paOutDefault(input, output) #default output for valueboxes
  
  paData <- eval(paData, envir = environment())
  
  pnData <- eval(pnData, envir = environment())
  
  #run button for example data
  observeEvent(input$paTest, paOut(input, output, data = percAgrTableExp)) #categories is 8 for the example
  
  observeEvent(input$pnTest, pnOut(input, output, data = percAgrPosNeg))
  
  
  #if run is pressed without userdata warning popup will be displayed
  #isolate hinders that anything happens when button is pressed without any
  #data uploaded.
  pa_react_btn <- eventReactive(input$paRun,
                         {T})
  pn_react_btn <- eventReactive(input$pnRun,
                                {T})
  
  observeEvent(pa_react_btn(),
               {
                 if (is.null(input$paInput)) {
                   btnPressWithoutData()
                 } else {
                   paOut(input, output, paData())
                 }
               })
  
  observeEvent(pn_react_btn(),
               {
                 if (is.null(input$pnInput)) {
                   btnPressWithoutData()
                 } else {
                   pnOut(input, output, pnData())
                 }
               })
  
  
  #-----------------------------------------------------------------------------
  #'*------------------------------- KAPPA ------------------------------------*
  #-----------------------------------------------------------------------------
  kappaOutDefault(input, output)
  kappaExampleTable(input, output)
  
  kappaData <- reactive({ #user input data
    read_csv(input$kappaInput$datapath)
  })
  
  observeEvent(input$kappaTest, kappaOut(input, output, kappaTableExample))
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
  observeEvent(input$kappaInput, kappaOut(input, output, kappaData()))
  
  #-----------------------------------------------------------------------------
  #'*--------------------------- SPEARMAN RHO ---------------------------------*
  #-----------------------------------------------------------------------------
  spearExp(input, output)
  spearOutDefault(input, output)
  
  ordinalData <- reactive({ #user input data
    read.csv(input$ordinalInput$datapath)
  })
  
  observeEvent(input$spearTest, spearOut(input, output, spearmanTableExample,
                                         'spearman'))
  
  observeEvent(input$spear_p_value, spear_no_exact_p())
  
  spear_react_btn <- eventReactive(input$spearRun,
                                {T})
  
  kendallW_react_btn <- eventReactive(input$kendRun,
                                     {T})
  
  observeEvent(spear_react_btn(),
               {
                 if (is.null(input$ordinalInput)) {
                   btnPressWithoutData()
                 } else {
                   spearOut(input, output, ordinalData(), 'spearman')
                 }
               })
  
  observeEvent(kendallW_react_btn(),
               {
                 if (is.null(input$ordinalInput)) {
                   btnPressWithoutData()
                 } else {
                   spearOut(input, output, ordinalData(), 'kendall')
                 }
               })
  
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  
  mainCol(input, output)
  #border-top-width: 20px;
  
  
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
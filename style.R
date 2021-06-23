navbarCol <- tags$style(HTML(
  '.skin-blue .main-header .navbar {transition:1s}
           .skin-blue .main-header .navbar:hover {background-color: dimgrey}'
))
bodyCol <- tags$style(HTML(
  '.content-wrapper {background-color: dimwhite;}'))

dropMenuStyle <- tags$style(HTML(
  '.tippy-tooltip.translucent-theme {background-color: #3c8dbc !important;}
          
  .tippy-tooltip.translucent-theme[data-placement^="left-start"] > .tippy-arrow {border-left-color: #3c8dbc;}
  
  .tippy-tooltip .table {background-color: #3c8dbc !important; color: white;}
  
  .tippy-tooltip tr:hover {background-color: #51BFFF !important;}
  
  '
))

boxCol <- tags$style(HTML(
  '.box.box{
          border-style: solid;
          border-bottom-color:white;
          border-left-color:white;
          border-right-color:white;
          border-top-color:white;
          background:white
          }'
))

tabCol <- tags$style(HTML(
  ".tabStyle .tab-content {
          background-color: white;
          color: black
          }
          
          .tabStyle .nav > li > a {
          background-color: white;
          color: black;
          }
          
          .tabStyle .nav > li > a {
          background-color: white;
          color: black;
          }
          
          .tabStyle .nav > li[class=active] > a {
          background-color: dimwhite;
          color: black;
          }
          
          .tabStyle .nav > li > a:hover {
          background-color: dimwhite;
          color: black;
          
          .tabSTyle .nav > li[class=active] > a:hover {
          background-color: dimwhite;
          color: black;
          }
          }"
))

histPlotCol <<- 'white'
histPlotText <<- 'black'


mainColLight <- function(input, output) {
  
  output$dark <- renderUI({
      list(
        navbarCol <- tags$style(HTML(
          '.skin-blue .main-header .navbar {transition:1s}
           .skin-blue .main-header .navbar:hover {background-color: dimgrey}'
        )),
        bodyCol <- tags$style(HTML(
          '.content-wrapper {background-color: dimwhite;}')),
        
        dropMenuStyle <- tags$style(HTML(
          '.tippy-tooltip.translucent-theme {background-color: #3c8dbc !important;}
          
          .tippy-tooltip.translucent-theme[data-placement^="left-start"] > .tippy-arrow {border-left-color: #3c8dbc;}
          
          .tippy-tooltip .table {background-color: #3c8dbc !important; color: white;}
          
          .tippy-tooltip tr:hover {background-color: #51BFFF !important;}
          
          '
        )),
        
        boxCol <- tags$style(HTML(
          '.box.box{
          border-style: solid;
          border-bottom-color:white;
          border-left-color:white;
          border-right-color:white;
          border-top-color:white;
          background:white
          }'
        )),
        
        tabCol <- tags$style(HTML(
          ".tabStyle .tab-content {
          background-color: white;
          color: black
          }
          
          .tabStyle .nav > li > a {
          background-color: white;
          color: black;
          }
          
          .tabStyle .nav > li > a {
          background-color: white;
          color: black;
          }
          
          .tabStyle .nav > li[class=active] > a {
          background-color: dimwhite;
          color: black;
          }
          
          .tabStyle .nav > li > a:hover {
          background-color: dimwhite;
          color: black;
          
          .tabSTyle .nav > li[class=active] > a:hover {
          background-color: dimwhite;
          color: black;
          }
          }"
        ))
      )
    
  })
}


mainColDark <- function(input, output) {
  
  output$dark <- renderUI({
      list(
        
        dropMenuStyle <- tags$style(HTML(
          '.tippy-tooltip.translucent-theme {background-color: #3c8dbc !important;}
          
          .tippy-tooltip.translucent-theme[data-placement^="left-start"] > .tippy-arrow {border-left-color: #3c8dbc;}
          
          .tippy-tooltip .table {background-color: #3c8dbc !important; color: white;}
          
          .tippy-tooltip tr:hover {background-color: #51BFFF !important;}
          
          '
        )),
        
        
        bodyCol <- tags$style(
        '.content-wrapper {background-color: #2E2E2E;}'),
        boxCol <- tags$style(
          '
          .box.box{
          border-style: solid;
          border-bottom-color:#404040;
          border-left-color:#404040;
          border-right-color:#404040;
          border-top-color:white;
          background:#404040;
          color: white;
          }
          
          .shiny-input-container{
          color:white;}'),
        
        tags$style('p {color:white;}
                   h1 {color:white;}
                   h2 {color:white;}
                   h3 {color:white;}
                   h4 {color:white;}
                   h5 {color:white;}
                   table {color:white;}
                   tr:hover {background-color:dimgrey;}'),
        
        
        tabCol <- tags$style(
          HTML(
            ".tabStyle .tab-content {
            background-color: #404040;
            color: black;
            border: none;}
    
            .tabStyle .nav {
              background-color: #404040;
              color: black;
              border: none;
            }

            .tabStyle .nav > li > a {
              background-color: #404040;
              color: white;
              border: none;
            }
            
            .tabStyle .nav > li[class=active] > a {
              background-color: dimgrey;
              color:white;
              border: none;
            }
            
            .tabStyle .nav > li > a:hover {
              background-color: dimgrey;
              color: white;
              border: none;
            }
            
            .tabStyle .nav > li[class=active] > a:hover {
              background-color: dimgrey;
              color: white;
              border: none;
            }"
          )
        ))
    
  })
}

# btn_hover <- function(input, output) {
#   output$bttnCol <- renderUI({
#     if (input$changCol %% 2 != 0) {
#       bttnCol <- tags$style(
#         '.btn {transition-duration:0.4s}
#           .btn:hover {
#           background-color:darkcyan;
#           color:white}'
#       )
#     }
#   })
# }

js_upload_msg_ordinalInput <- "
Shiny.addCustomMessageHandler('upload_msg', function(msg) {
  var target = $('#ordinalInput_progress').children()[0];
  target.innerHTML = msg;
});
"

measure_title_style <- 'text-align: center; padding: 0;'

centerText <- 'text-align: center;'

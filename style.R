navbarCol <- tags$style(HTML(
  '.skin-blue .main-header .navbar {transition:1s}
           .skin-blue .main-header .navbar:hover {background-color: dimgrey}'
))
bodyCol <- tags$style(HTML(
  '.content-wrapper {background-color: dimwhite;}'))

dropMenuStyle <- tags$style(HTML(
  '.tippy-tooltip.translucent-theme {background-color: #3c8dbc !important;}
          
  .tippy-tooltip.translucent-theme[data-placement^="left-start"] > .tippy-arrow {border-left-color: #3c8dbc;}
  
  .tippy-tooltip.translucent-theme[data-placement^="bottom"] > .tippy-arrow {border-bottom-color: #3c8dbc;}
  
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
    ".datatables.html-widget.html-widget-output.shiny-bound-output {
      background-color: white;
      color: black;
    }
    
    table.dataTable.display tbody tr.odd {
      background-color: white;
    }
    
    table.dataTable.display tbody tr.even {
      background-color: white;
    }
    
    table.dataTable.display tbody tr:hover {
      background-color: lightgrey;
    }
    
    
    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .select {
      color: black;
      border: none;
    }
    
    
    .dataTables_wrapper select, .dataTables_wrapper input {
      background-color: lightgrey !important;
      border: none;
      color: black !important;
    }
    
    .dataTables_wrapper .dataTables_paginate .paginate_button.disabled, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:active {
      color: black !important;
      border: none;
    }
    
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      color: black !important;
      border: none;
    }
    
    .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
      color: white;
      background: dimgrey;
      border: none;
    }
    
    .dataTables_wrapper .dataTables_paginate .paginate_button.current, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
      color: black;
      background: lightgrey;
      border: none;
    }
    
    table.dataTable thead th, table.dataTable thead td {
      border: none;
    }
    
    table.dataTable.no-footer {
      border-bottom-color: #404040;
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
          
          .tippy-tooltip.translucent-theme[data-placement^="bottom"] > .tippy-arrow {border-bottom-color: #3c8dbc;}
          
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
          ".datatables.html-widget.html-widget-output.shiny-bound-output {
              background-color: white;
              color: black;
            }
            
            table.dataTable.display tbody tr.odd {
              background-color: white;
            }
            
            table.dataTable.display tbody tr.even {
              background-color: white;
            }
            
            table.dataTable.display tbody tr:hover {
              background-color: lightgrey;
            }
            
            
            .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .select {
              color: black;
              border: none;
            }
            
            
            .dataTables_wrapper select, .dataTables_wrapper input {
              background-color: #3B3B3B24 !important;
              border: none;
              color: black !important;
            }
          
            .dataTables_wrapper select:hover,  {
              background-color: #3B3B3B24 !important;
              border: none;
              color: black !important;
            }
            
            .dataTables_wrapper .dataTables_paginate .paginate_button.disabled, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:active {
              color: black !important;
              border: none;
            }
            
            .dataTables_wrapper .dataTables_paginate .paginate_button {
              color: black !important;
              border: none;
            }
            
            .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
              color: white;
              background: dimgrey;
              border: none;
            }
            
            .dataTables_wrapper .dataTables_paginate .paginate_button.current, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
              color: black;
              background: lightgrey;
              border: none;
            }
            
            table.dataTable thead th, table.dataTable thead td {
              border: none;
            }
            
            table.dataTable.no-footer {
              border-bottom-color: #404040;
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
          
          .tippy-tooltip.translucent-theme[data-placement^="bottom"] > .tippy-arrow {border-bottom-color: #3c8dbc;}
          
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
            ".datatables.html-widget.html-widget-output.shiny-bound-output {
                background-color: #404040;
                color: white;
              }
              
              table.dataTable.display tbody tr.odd {
                background-color: #404040;
              }
              
              table.dataTable.display tbody tr.even {
                background-color: #404040;
              }
              
              table.dataTable.display tbody tr:hover {
                background-color: dimgrey;
              }
              
              
              .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .select {
                color: white;
                border: none;
              }
              
              
              .dataTables_wrapper select, .dataTables_wrapper input {
                background-color: dimgrey !important;
                border: none;
                color: white !important;
              }
              
              .dataTables_wrapper .dataTables_paginate .paginate_button.disabled, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:active {
                color: white !important;
                border: none;
              }
              
              .dataTables_wrapper .dataTables_paginate .paginate_button {
                color: white !important;
                border: none;
              }
              
              .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
                color: white;
                background: dimgrey;
                border: none;
              }
              
              .dataTables_wrapper .dataTables_paginate .paginate_button.current, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                color: black;
                background: white;
                border: none;
              }
              
              table.dataTable thead th, table.dataTable thead td {
                border: none;
              }
              
              table.dataTable.no-footer {
                border-bottom-color: #404040;
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

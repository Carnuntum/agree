
other_awg <- tabItem(
  tabName = 'other_awg',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    id = 'awgDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("Reliability Within Group Index")
                  ),
                  hidden(
                    div(id = 'awgDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = awg_docum_text,
                                            width = NULL,
                                            style = 'text-align:center; padding: 0;')
                                 )
                        )
                    )
                  )
  )
  ),
  fluidRow(
    column(
      width = 5,
      offset = 1,
      fluidRow(
        box(
          width = NULL,
          height = '105px',
          p(file_upload_text),
          style = centerText
        )
      ),
      fluidRow(
        box(
          width = NULL,
          height = '105px',
          p(file_struct_text),
          look_down,
          style = centerText
        )
      )
    )
    ,
    column(
      width = 5,
      box(
        width = NULL,
        fileInput(inputId = 'awgInput',
                  label = 'Browse for .csv files'),
        actionButton(
          inputId = 'awgRun',
          label = 'calculate'
        ),
        style = centerText
      )
    )
  ),
  fluidRow(
    class = 'tabStyle',
    column(
      width = 5,
      offset = 1,
      style = 'padding: 0px;',
      uiOutput('ui_awg')
    ),
    
    column(width = 5,
           shinyWidgets::dropMenu(
             div(id = 'awgDrop',
                 fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                          column(
                            width = 12,
                            valueBoxOutput(outputId = 'awg', width = NULL)
                          )
                 )
             ),
             HTML(kableExtra::kable(t(awg_output_description)) %>% 
                    kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri')),
             trigger = 'mouseenter',
             theme = 'translucent',
             placement = 'left-start')
    )
  )
)




#-------------------------------------------------------------------------------
#'*--------------- CALCULATED OUTPUT FOR PERCENT AGREEMENT --------------------*
#-------------------------------------------------------------------------------
#chi output for corrected test
awgOut <- function(input, output, data) {
  
  tryCatch({
    
    vals_awg <- list('vals' = warning_handler(awg(data[,2:ncol(data)], data[,1])),
                     'warn' = msg)
    
    l_awg <<- lapply(vals_awg$vals, as.data.frame)
    
    M <- mean(unlist(data[,2:ncol(data)]))
    L <- min(data)
    H <- max(data)
    k <- ncol(data)
    
    vals_awg$vals$mean <- M
    min <- (L * (k - 1) + H) / k
    max <- (H * (k - 1) + L) / k
    vals_awg$vals$`valid mean interval` <- paste0('[', min, ', ', max, ']')
    vals_awg$vals$interpretable <- ifelse(M < min || M > max, 'no', 'yes')
    
    d_awg <- t(as.data.frame(vals_awg$vals))
    
    if(dim(d_awg)[2] > 10) {
      d_awg <- t(d_awg)
    }
    
    output$awg <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(
          kableExtra::kable(d_awg, format = 'html') %>% 
            kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri'),
          
        ),
        div(
          if(!is.null(msg)) {
            p(HTML(paste0(
              circleButton(inputId = 'gg',
                           icon = icon("exclamation"),
                           size = 's'),
              br()
            )))
          },
          style = centerText
        ),
        div(
          downloadButton(outputId = 'awgFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'awg')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'awg')
    print(w)
  })
}



#for within rater agreement
# dat <- as.data.frame((sav_data[,3:8]))
# 
# dims <- dim(dat)
# 
# dat <- pivot_longer(dat, cols = 1:dims[2], names_to = 'subjects', values_to = 'vals')
# 
# dat$subjects <- as.numeric(as.character(factor(dat$subjects, labels = 1:dims[2])))
# 
# dat <- as.data.frame(dat)
# 
# summary(rwg(dat$vals, dat$subjects))
# 
# (sig <- (max(xmp_poly2)^2 - 1) / 12)
# (v <- mean(diag(var(xmp_poly2))))
# (J <- dims[1])
# 
# (J * (1 - (v / sig))) /
#   (J * ((1 - (v / sig)) + (v / sig)))
# 
# 
# finn <- data.frame(
#   'r1' = c(5,4,3,5,4,3,5,4,3,5),
#   'r2' = c(4,3,5,4,3,5,4,3,5,5),
#   'r3' = c(3,5,4,3,5,4,3,5,4,5)
# )
# 
# finn3 <- data.frame(
#   'r1' = c(5,5,5,4,5,5,4,5,4,5),
#   'r2' = c(4,4,4,4,4,5,4,5,5,5),
#   'r3' = c(4,3,5,5,3,4,5,4,3,5)
# )
# 
# 
# s <- 0
# for(i in 1:10) {
#   m <- mean(unlist(finn3[i,]))
#   for(j in 1:3) {
#     s <- s + ((finn3[i,j] - m)^2)
#   }
# }
# 
# 
# rwgj <- function(ratings) {
#   varO <- mean(apply(ratings, 1, var))
#   J <- max(ratings)
#   sig <- (J^2 - 1) / 12
#   
#   est <- (J * (1 - (varO / sig))) /
#     (J * (1 - (varO / sig)) + (varO / sig))
#   
#   return(est)
# }
# 
# #for rwg.j and rwg functions, items/subjecs = columns, raters = rows, additional
# #row for group ids has to be added or be existent for it to work
# 
# 
# sav_data <- read.csv('example_data/rwg_dat.csv')
# haven::write_sav(as.data.frame(sav_data), 'example_data/rwg_sav.sav')

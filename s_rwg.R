
other_rwg <- tabItem(
  tabName = 'other_rwg',
  fluidRow(column(width = 10, 
                  offset = 1,
                  style = 'padding-left: 0px; padding-right: -5px;',
                  box(
                    id = 'rwgDocum',
                    width = NULL,
                    style = measure_title_style,
                    h3("Reliability Within Group Index")
                  ),
                  hidden(
                    div(id = 'rwgDocumBox',
                        fluidRow(class = 'documRow',
                                 column(width = 12,
                                        offset = 0,
                                        box(title = rwg_docum_text,
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
        fileInput(inputId = 'rwgInput',
                  label = 'Browse for .csv files'),
        actionButton(
          inputId = 'rwgRun',
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
      uiOutput('ui_rwg')
    ),
    
    column(width = 5,
           shinyWidgets::dropMenu(
             div(id = 'rwgDrop',
                 fluidRow(class = 'style_valuebox_OUTPUT_cyan',
                          column(
                            width = 12,
                            valueBoxOutput(outputId = 'rwg', width = NULL)
                          )
                 )
             ),
             HTML(kableExtra::kable(t(rwg_output_description)) %>% 
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
rwgOut <- function(input, output, data) {
  
  tryCatch({
    
    bfun_rwg <- function(d, i) {
      dat <- d[i,]
      return(rwg(dat[,2:ncol(dat)], dat[,1])$rwg)
    }
    
    bfun_rwgj <- function(d, i) {
      dat <- d[i,]
      return(mean(rwg.j(dat[,2:ncol(dat)], dat[,1])$rwg.j))
    }
    
    if(ncol(data) == 2) {
      vals_rwg <- list('vals' = warning_handler(list('rwg' = rwg(data[,2:ncol(data)], data[,1]))),
                       'warn' = msg)
      
      b <- warning_handler(boot::boot(data, bfun_rwg, 2000))
      ci <- warning_handler(boot::boot.ci(b, type = 'perc')$percent[4:5])
      
      vals_rwg$vals$lb <- ci[1]
      vals_rwg$vals$ub <- ci[2]
      
      full_rwg <- as.data.frame(rwg(data[,2:ncol(data)], data[,1]))
      
      #vals_rwg$vals <- as.data.frame(append(as.list(vals_rwg$vals), c('lb' = ci[1], 'ub' = ci[2])))
      
    } else {
      vals_rwg <- list('vals' = warning_handler(list('rwg' = mean(rwg.j(data[,2:ncol(data)], data[,1])$rwg.j))),
                       'warn' = msg)
      
      full_rwg <- (as.data.frame(rwg.j(data[,2:ncol(data)], data[,1])))
      
      b <- warning_handler(boot::boot(data, bfun_rwgj, 2000))
      ci <- warning_handler(boot::boot.ci(b, type = 'perc')$percent[4:5])
      
      vals_rwg$vals$lb <- ci[1]
      vals_rwg$vals$ub <- ci[2]
      
      #vals_rwg$vals <- as.data.frame(append(as.list(vals_rwg$vals), c('lb' = ci[1], 'ub' = ci[2])))
    }
    
    l_rwg <<- lapply(vals_rwg$vals, as.data.frame)
    
    d_rwg <- t(as.data.frame(vals_rwg$vals))
    
    if(dim(d_rwg)[2] > 10) {
      d_rwg <- t(d_rwg)
    }
    
    output$rwg <- renderValueBox({
      
      valueBox(
        subtitle = p(HTML(paste0(
          kableExtra::kable(d_rwg, format = 'html') %>% 
            kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri'),
          br(),
          kableExtra::kable(full_rwg, format = 'html') %>% 
            kableExtra::kable_styling('basic', font_size = 15, html_font = 'calibri')
        )),
        div(
          if(!is.null(msg)) {
            p(HTML(paste0(
              circleButton(inputId = 'test1'),
                           icon = icon("exclamation"),
                           size = 's'),
              br()
            ))
          },
          style = centerText
        ),
        div(
          downloadButton(outputId = 'rwgFullDown',
                         label = 'Full Results'),
          style = centerText
        )),
        value = ''
      )
    })
  }, error = function(e) {
    invalid_data(output, 'rwg')
    print(e)
  }, warning = function(w) {
    invalid_data(output, 'rwg')
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

#-------------------------------------------------------------------------------
#'*------------------------ GERNERAL SET FUNCTIONS ----------------------------*
#-------------------------------------------------------------------------------
#create tabset panel with one tabpanel inside for setting the example/user data
#in the output tabset panel
create_tabset <- function(output,
                        out_id,
                        id,
                        tabTitle,
                        boxTitle,
                        tableId,
                        downId,
                        btnInputId) {


  tab <- tabPanel(
    title = tabTitle,
    box(
      title = boxTitle,
      width = NULL,
      tableOutput(tableId),
      downloadButton(
        outputId = downId,
        label = '',
        style = 'float: right;'
      ),
      actionButton(
        inputId = btnInputId,
        label = 'test run',
        style = 'float: right;'
      )
    )
  )
  
  output[[out_id]] <- renderUI({
    tabsetPanel(id = id,
                tab)
  })

}

make_xmp_table <- function(output, out_id, data) {
  output[[out_id]] <- function() {
    tryCatch({
      kableExtra::kable(data, format = 'html') %>%
        kableExtra::kable_styling('basic')
    }, error = function(e) {
      print('make_xmp_table - error')
      print(e)
      return('invalid data, please look at your data carefully')
    }, warning = function(w) {
      print('make_xmp_table - warning')
      print(w)
      return('invalid data, please look at your data carefully')
    })
  }
}

#update a given tabsetpanel with title and box title
set_tabset <- function(output,
                       out_id,
                       id,
                       tabTitle = 'Example',
                       boxTitle = 'Data',
                       tableId,
                       downId,
                       btnInputId,
                       data) {
  create_tabset(output, out_id, id, tabTitle, boxTitle, tableId, downId, btnInputId)
  make_xmp_table(output, tableId, data = data)
}

#make example output


#-------------------------------------------------------------------------------
#'*----------------------- DATA EXAMPLE TABLES --------------------------------*
#-------------------------------------------------------------------------------
#polychoric extrawurst
t <- tabPanel(
  title = 'Example 2',
  box(
    title = 'Data',
    width = NULL,
    tableOutput('tab_polyc2'),
    downloadButton(
      outputId = 'down_polyc2',
      label = '',
      style = 'float: right;'
    ),
    actionButton(
      inputId = 'test_polyc2',
      label = 'test run',
      style = 'float: right;'
    )
  )
)

polyAppendTab <- function(session) {
  session$onFlushed(function() {
    appendTab('id_polyc', t)
    updateTabsetPanel(session, "id_polyc")
  })
}

ExampleTables <- function(input, output) {
  
  #scotts pi
  set_tabset(output, out_id = 'ui_pi', id = 'id_pi', tableId = 'tab_pi', downId = 'down_pi',
             btnInputId = 'test_pi', data = xmp_spear)
  
  #COHENS KAPPA
  set_tabset(output, out_id = 'ui_cohen', id = 'id_cohen', tableId = 'tab_cohen', downId = 'down_cohen',
             btnInputId = 'test_cohen', data = xmp_pa)
  
  #ADJUSTED RAND INDEX
  set_tabset(output, out_id = 'ui_randIndex', id = 'id_randIndex', tableId = 'tab_randIndex', downId = 'down_randIndex',
             btnInputId = 'test_randIndex', data = xmp_spear)
  
  #CONGERS MULTIRATER KAPPA
  set_tabset(output, out_id = 'ui_conger', id = 'id_conger', tableId = 'tab_conger', downId = 'down_conger',
             btnInputId = 'test_conger', data = xmp_poly2)
  
  #FLEISS KAPPA
  set_tabset(output, out_id = 'ui_fleiss', id = 'id_fleiss', tableId = 'tab_fleiss', downId = 'down_fleiss',
             btnInputId = 'test_fleiss', data = xmp_poly2)
  
  #BRENNAN PREDIGERS KAPPA
  set_tabset(output, out_id = 'ui_brennan', id = 'id_brennan', tableId = 'tab_brennan', downId = 'down_brennan',
             btnInputId = 'test_brennan', data = xmp_poly1)
  
  #RWG
  set_tabset(output, out_id = 'ui_rwg', id = 'id_rwg', tableId = 'tab_rwg', downId = 'down_rwg',
             btnInputId = 'test_rwg', data = xmp_rwg)
  
  #AWG
  set_tabset(output, out_id = 'ui_awg', id = 'id_awg', tableId = 'tab_awg', downId = 'down_awg',
             btnInputId = 'test_awg', data = xmp_rwg)
  
  #BANGDIWALA B
  set_tabset(output, out_id = 'ui_bangdiwala', id = 'id_bangdiwala', tableId = 'tab_bangdiwala', downId = 'down_bangdiwala',
             btnInputId = 'test_bangdiwala', data = as.data.frame(xmp_kappa))
  
  #AICKIN ALPHA
  set_tabset(output, out_id = 'ui_aickin', id = 'id_aickin', tableId = 'tab_aickin', downId = 'down_aickin',
             btnInputId = 'test_aickin', data = as.data.frame(xmp_spear))
  
  #BYRTS KAPPA
  set_tabset(output, out_id = 'ui_byrt', id = 'id_byrt', tableId = 'tab_byrt', downId = 'down_byrt',
             btnInputId = 'test_byrt', data = xmp_icc)
  
  #IOTA
  set_tabset(output, out_id = 'ui_iota', id = 'id_iota', tableId = 'tab_iota', downId = 'down_iota',
             btnInputId = 'test_iota', data = xmp_icc)
  
  #GWET
  set_tabset(output, out_id = 'ui_gwet', id = 'id_gwet', tableId = 'tab_gwet', downId = 'down_gwet',
             btnInputId = 'test_gwet', data = xmp_icc)
  
  #ENTROPY
  set_tabset(output, out_id = 'ui_entropy', id = 'id_entropy', tableId = 'tab_entropy', downId = 'down_entropy',
             btnInputId = 'test_entropy', data = xmp_icc)
  
  #VON EYE K
  set_tabset(output, out_id = 'ui_eye', id = 'id_eye', tableId = 'tab_eye', downId = 'down_eye',
             btnInputId = 'test_eye', data = xmp_icc)
  
  #AD INDEX
  set_tabset(output, out_id = 'ui_ad', id = 'id_ad', tableId = 'tab_ad', downId = 'down_ad',
             btnInputId = 'test_ad', data = xmp_icc)
  
  #A KAPPA
  set_tabset(output, out_id = 'ui_akappa', id = 'id_akappa', tableId = 'tab_akappa', downId = 'down_akappa',
             btnInputId = 'test_akappa', data = xmp_icc)
  
  #FREE KAPPA
  set_tabset(output, out_id = 'ui_freeKappa', id = 'id_freeKappa', tableId = 'tab_freeKappa', downId = 'down_freeKappa',
             btnInputId = 'test_freeKappa', data = xmp_pn)
  
  #INFORMATION AGREEMENT
  set_tabset(output, out_id = 'ui_infoAgree', id = 'id_infoAgree', tableId = 'tab_infoAgree', downId = 'down_infoAgree',
             btnInputId = 'test_infoAgree', data = xmp_pn)
  
  #ICC
  set_tabset(output, out_id = 'ui_icc', id = 'id_icc', tableId = 'tab_icc', downId = 'down_icc',
             btnInputId = 'test_icc', data = xmp_icc)
  
  #OMEGA
  set_tabset(output, out_id = 'ui_omega', id = 'id_omega', tableId = 'tab_omega', downId = 'down_omega',
             btnInputId = 'test_omega', data = xmp_omega)
  
  #KRIPPENDORFF
  set_tabset(output, out_id = 'ui_kripp', id = 'id_kripp', tableId = 'tab_kripp', downId = 'down_kripp',
             btnInputId = 'test_kripp', data = xmp_kripp)
  
  #replace
  cccTemp <<- round(xmp_ccc[1:10,], 3)
  lapply(cccTemp, as.character)
  cccTemp[[1]][10] = '...'
  cccTemp[[2]][10] = '...'
  
  #replace
  set_tabset(output, out_id = 'ui_occc', id = 'id_occc', tableId = 'tab_occc', downId = 'down_occc',
             btnInputId = 'test_occc', data = cccTemp)
  
  #make_xmp_table(output, 'expccc1', cccTemp)
  
}

measureDocumentationShow <- function(docum, documBox) {
  shinyjs::onclick(id = docum, shinyjs::toggle(id = documBox, anim = T))
}


#-------------------------------------------------------------------------------
#'*------------------------- DEFAULT OUTPUT FOR ALL ---------------------------*
#-------------------------------------------------------------------------------

defaultOutAll <- function(input, output) {
  

#'*---------------------------------- PI --------------------------------------*
#-------------------------------------------------------------------------------

  output$pi1 <- renderValueBox({
      valueBox(value = h4('Output'), '')
  })
  
#'*----------------------------- COHEN KAPPA ----------------------------------*
#-------------------------------------------------------------------------------
output$cohen <- renderValueBox({
  valueBox(value = h4('Output',
                      style = 'text-align: center;
                                padding: 15px;'), '')
})

#'*------------------------------ RAND INDEX ----------------------------------*
#-------------------------------------------------------------------------------


output$randIndex <- renderValueBox({
  valueBox(value = h4('Output',
                      style = 'text-align: center;
                                  padding: 15px;'), '')
})

#'*---------------------------- CONGERS KAPPA ---------------------------------*
#-------------------------------------------------------------------------------
output$conger <- renderValueBox({
  valueBox(value = h4("Output",
                      style = 'text-align: center;
                                  padding: 15px;'), '')
})
  
#'*--------------------------- FLEISS KAPPA -----------------------------------*
#-------------------------------------------------------------------------------

  
  output$fleiss <- renderValueBox({
    valueBox(value = h4('Output',
                        style = 'text-align: center;
                                  padding: 15px;'), '')
  })
  
  #'*------------------------- BRENNAN PREDIGER KAPPA ---------------------------*
  #-------------------------------------------------------------------------------
  output$brennan <- renderValueBox({
    valueBox(value = h4("Output",
                        style = 'text-align: center;
                                  padding: 15px;'), '')
  })
  

  
#'*---------------------------------- RWG -------------------------------------*
#-------------------------------------------------------------------------------
  output$rwg <- renderValueBox({
    valueBox(value = h4('Output',
                        style = 'text-align: center;
                                  padding: 15px;'), '')
  })
  
#'*---------------------------------- AWG -------------------------------------*
#-------------------------------------------------------------------------------
output$awg <- renderValueBox({
  valueBox(value = h4('Output',
                      style = 'text-align: center;
                                padding: 15px;'), '')
})

#'*---------------------------- BANGDIWALA B ----------------------------------*
#-------------------------------------------------------------------------------
output$bangdiwala <- renderValueBox({
  valueBox(value = h4('Output',
                      style = 'text-align: center;
                                padding: 15px;'), '')
})

#'*---------------------------- AICKIN ALPHA ----------------------------------*
#-------------------------------------------------------------------------------
output$aickin <- renderValueBox({
  valueBox(value = h4('Output',
                      style = 'text-align: center;
                                padding: 15px;'), '')
})

#'*----------------------------- BYRT KAPPA -----------------------------------*
#-------------------------------------------------------------------------------
output$byrt <- renderValueBox({
  valueBox(value = h4('Output',
                      style = 'text-align: center;
                                padding: 15px;'), '')
})

#'*-------------------------------- IOTA --------------------------------------*
#-------------------------------------------------------------------------------
output$iota <- renderValueBox({
  valueBox(value = h4('Output',
                      style = 'text-align: center;
                                padding: 15px;'), '')
})

#'*------------------------------- GWETS AC -----------------------------------*
#-------------------------------------------------------------------------------
output$gwet <- renderValueBox({
  valueBox(value = h4('Output',
                      style = 'text-align: center;
                                padding: 15px;'), '')
})

#'*---------------------------- ENTROPY INDEX ---------------------------------*
#-------------------------------------------------------------------------------
# output$entropy <- renderValueBox({
#   valueBox(value = h4('Output',
#                       style = 'text-align: center;
#                                 padding: 15px;'), '')
# })

#'*----------------------------- VON EYE KAPPA --------------------------------*
#-------------------------------------------------------------------------------
output$eye <- renderValueBox({
  valueBox(value = h4('Output',
                      style = 'text-align: center;
                                padding: 15px;'), '')
})

#'*------------------------------- AD INDEX -----------------------------------*
#-------------------------------------------------------------------------------
output$ad <- renderValueBox({
  valueBox(value = h4('Output',
                      style = 'text-align: center;
                                padding: 15px;'), '')
})

#'*-------------------------------- A KAPPA -----------------------------------*
#-------------------------------------------------------------------------------
output$akappa <- renderValueBox({
  valueBox(value = h4('Output',
                      style = 'text-align: center;
                                padding: 15px;'), '')
})

#'*----------------------------- FREE KAPPA -----------------------------------*
#-------------------------------------------------------------------------------
output$freeKappa <- renderValueBox({
  valueBox(value = h4('Output',
                      style = 'text-align: center;
                                padding: 15px;'), '')
})

#'*----------------------- INFORMATION AGREEMENT ------------------------------*
#-------------------------------------------------------------------------------
output$infoAgree <- renderValueBox({
  valueBox(value = h4('Output',
                      style = 'text-align: center;
                                padding: 15px;'), '')
})
  
#'*--------------------------------- ICC --------------------------------------*
#-------------------------------------------------------------------------------
  output$icc1 <- renderValueBox({
    valueBox(value = h4("Output",
                        style = 'text-align: center;
                                padding: 15px;'), '')
  })
  
#'*------------------------------- KRIPP --------------------------------------*
#-------------------------------------------------------------------------------
  output$kripp1 <- renderValueBox({
    valueBox(value = h4("Output",
                        style = 'text-align: center;
                                padding: 15px;'), '')
  })

#'*-------------------------------- OCCC --------------------------------------*
#-------------------------------------------------------------------------------
  output$occc <- renderValueBox({
    valueBox(value = h4("Output",
                        style = 'text-align: center;
                                padding: 15px;'), '')
  })
  
  
}



#-------------------------------------------------------------------------------
#'*--------------------- DEFAULT OUTPUT FOR ALL END ---------------------------*
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#'*------------------------------ RAND INDEX ----------------------------------*
#-------------------------------------------------------------------------------
randMain <- function(data) {
  data <- na.omit(data)
  
  vals_boot <- boot::boot(data, statistic = function(d, i) {
    dat <- d[i,]
    return(mclust::adjustedRandIndex(dat[,1], dat[,2]))
  }, R = 2000)
  
  vals_ci <- boot::boot.ci(vals_boot, type = 'perc')$percent[4:5]
  
  return(list('est' = vals_boot$t0,
              'lb' = vals_ci[1],
              'ub' = vals_ci[2]))
}


#-------------------------------------------------------------------------------
#'*--------------------------------- ICC --------------------------------------*
#-------------------------------------------------------------------------------


iccMain <- function(input, output, data, test) {
  
  choices <- c(input$iccChoices, 'agreement', input$iccChoices2)
  
  
  if(test) {
    if(length(choices) != 3) {
      tooManyIcc(output)
    } else {
    return(irr::icc(ratings = data, model = choices[1], 
                    type = choices[2], unit = choices[3]))
    }
  } else {
    
    if(length(choices) != 3) {
      tooManyIcc(output)
    } else {
      
      tryCatch({
        
        return(irr::icc(ratings = data, model = choices[1], 
                        type = choices[2], unit = choices[3]))
        
      }, error = function(e) {
        print('src_funtion_icc_e')
        print(e)
        iccErrOut(output)
      }, warning = function(w) {
        print('src_function_icc_w')
        print(w)
        iccErrOut(output)
      })
      
    }
  }
}

#-------------------------------------------------------------------------------
#'*------------------------------- OMEGA --------------------------------------*
#-------------------------------------------------------------------------------
isolate(omegaMain <- function(input, output, data) {
  
  isolate(choices <- tolower(c(input$omegaChoices, input$omegaChoices1, input$omegaChoices2)))
  
  tryCatch({
    if(length(choices) != 3) {
      tooManyOmega()
    }
    else {
      return(sklarsomega::sklars.omega(data, level = choices[1],
                                       confint = choices[2],
                                       control = list(
                                         bootit = 1000,
                                         dist = choices[3])
                                       )
             )
    }
  }, error = function(e) {
    print(e)
    globalOmegaWarn <<- conditionMessage(e)
  }, warning = function(w) {
    print(w)
    globalOmegaWarn <<- conditionMessage(w)
  })
})

#-------------------------------------------------------------------------------
#'*------------------------------- KRIPP --------------------------------------*
#-------------------------------------------------------------------------------

krippMain <- function(input, output, data) {
  tryCatch({
    choice <- input$krippChoice
    
    test <- krippendorffsalpha::krippendorffs.alpha(data, level = choice, control = list(bootit = 1000, nodes = 2))
    
    
  }, error = function(e) {
    print(e)
  }, warning = function(w) {
    print(w)
  })
}

#-------------------------------------------------------------------------------
#'*--------------------------------- CCC --------------------------------------*
#-------------------------------------------------------------------------------

cccMain <- function(input, output, data) {
  choice <- input$cccChoice
  
  tryCatch({
    test <- DescTools::CCC(x = data[,1], y = data[,2], ci = choice,
                           conf.level = 0.95, na.rm = T)
    
    
  }, error = function(e) {
    print('src_function_ccc_e')
    print(e)
  }, warning = function(w) {
    ccc_global_warnings <<- warnings()
    print('src_function_ccc_w')
    print(w)
  })
}


#-------------------------------------------------------------------------------
#'*------------------------ AICKINS ALPHA ------------------------------------'*
#-------------------------------------------------------------------------------

aickinAlpha <- function(n, d=diag(1, nrow=nrow(n), ncol=ncol(n)), epsilon=1e-7, level=0.95)
{
  # Version 1.0 (March 2013)
  #
  # This function computes the alpha coefficient described in:
  # Maximum Likelihood Estimation of Agreement in the Constant Predictive Probability Model, and Its Relation to Cohen-s Kappa
  # Mikel Aickin
  # Biometrics 46, 293-302, June 1990.
  #
  #
  # Function developed by 
  # Lawrence Joseph and Patrick B?lisle
  # Division of Clinical Epidemiology
  # Montreal General Hospital
  # Montreal, Qc, Can
  #
  # patrick.belisle@rimuhc.ca
  # http://www.medicine.mcgill.ca/epidemiology/Joseph/PBelisle/Aickin-Alpha-Agreement-R.html
  #
  # Please refer to our webpage for details on each argument.
  if(ncol(n) > 2) stop('more than 2 raters!')
  
  if(max(n) > 1000) stop('data structure is too big to calculate') 
  
  n <- table(factor(n[,1], min(n):max(n)), factor(n[,2], min(n):max(n)))
  d <- diag(1, nrow=nrow(n), ncol=ncol(n))
  
  if (any(dim(n) != dim(d))) stop("n and d must be of equal dimensions.")
  if (diff(dim(n)) != 0) stop("n and d must be square matrices. are there more than 2 raters?")
  
  m <- nrow(n)
  n <- n + 1/(m^2)
  J <- rep(1, m)
  ssize <- sum(n)
  A <- sum(n*d)
  p0 <- A/ssize
  rows.tot <- as.vector(n%*%matrix(J, ncol=1))
  cols.tot <- as.vector(matrix(J, nrow=1)%*%n)
  pr <- rows.tot/ssize
  pc <- cols.tot/ssize
  s <- sum(matrix(pc, nrow=m, ncol=m, byrow=T)*pr*d)
  alpha <- (p0 - s) / (1 - s)
  continue <- T
  
  while (continue)
  {
    previous.alpha <- alpha
    pr.denominator <- ssize * (1 - alpha + alpha * as.vector(d%*%matrix(pc, ncol=1)) / s)
    pr <- rows.tot/pr.denominator
    pr[1] <- 1- sum(pr[-1])
    pc.denominator <- ssize * (1 - alpha + alpha * as.vector(matrix(pr, nrow=1)%*%d) / s)
    pc <- cols.tot/pc.denominator
    pc[1] <- 1- sum(pc[-1])
    s <- sum(matrix(pc, nrow=m, ncol=m, byrow=T)*pr*d)
    alpha <- (p0 - s) / (1 - s)
    
    continue <- abs(alpha-previous.alpha) > epsilon
  }
  
  prdiff <- pr[-1] - pr[1] # m-1 x 1
  pcdiff <- pc[-1] - pc[1] # m-1 x 1
  
  d2L.da2 <- - ssize * (1-s)/(1-alpha)/((1-alpha)*s+alpha)
  R <- alpha/s/((1-alpha)*s+alpha)
  U <- 1/alpha - 1
  d2L.dadpri <- - A * pcdiff * ((ssize/A)^2) # m-1 x 1
  d2L.dadpcj <- - A * prdiff * ((ssize/A)^2) # m-1 x 1
  
  d2L.dpridprj <- -sum(n[1,])/(pr[1]^2) + A*(2*s*U+1)*R*R*matrix(pcdiff, ncol=1) %*% matrix(pcdiff, nrow=1) # m-1 x m-1
  d2L.dpri2 <- -rows.tot[-1]/(pr[-1]^2) - rows.tot[1]/(pr[1]^2) + A*(2*s*U + 1)*R*R*(pcdiff^2)
  diag(d2L.dpridprj) <- d2L.dpri2
  
  d2L.dpridpcj <- -A*R + A*(2*s*U+1)*R*R* matrix(pcdiff, ncol=1) %*% matrix(prdiff, nrow=1) # m-1 x m-1
  d2L.dpridpci <- -2*A*R + A*(2*s*U+1)*R*R*prdiff*pcdiff
  diag(d2L.dpridpcj) <- d2L.dpridpci
  
  d2L.dpcidpcj <- -sum(n[,1])/(pc[1]^2) + A*(2*s*U+1)*R*R*matrix(prdiff, ncol=1) %*% matrix(prdiff, nrow=1) # m-1 x m-1
  d2L.dpci2 <- -cols.tot[-1]/(pc[-1]^2) - cols.tot[1]/(pc[1]^2) + A*(2*s*U + 1)*R*R*(prdiff^2)
  diag(d2L.dpcidpcj) <- d2L.dpci2
  
  matrix.second.derivatives.top <- matrix(c(d2L.da2, d2L.dadpri, d2L.dadpcj), nrow=1)
  matrix.second.derivatives.middle <- cbind(matrix(d2L.dadpri, ncol=1), d2L.dpridprj, d2L.dpridpcj)
  matrix.second.derivatives.bottom <- cbind(matrix(d2L.dadpcj, ncol=1), t(d2L.dpridpcj), d2L.dpcidpcj)
  matrix.second.derivatives <- rbind(matrix.second.derivatives.top, matrix.second.derivatives.middle, matrix.second.derivatives.bottom)
  
  parms.cov.matrix <- solve(-matrix.second.derivatives)
  alpha.sd <- sqrt(parms.cov.matrix[1])
  z <- qnorm((1+level)/2)
  lcl <- alpha - z*alpha.sd
  ucl <- alpha + z*alpha.sd
  
  list(alpha=alpha, lcl=lcl, ucl=ucl)
}

#-------------------------------------------------------------------------------
#'*------------------------------ BYRTS KAPPA --------------------------------'*
#-------------------------------------------------------------------------------
byrtKappa <- function(data) {
  
  d <- na.omit(data)
  if(any(dim(d) == 0)) stop('no data to analyse. too many NAs?')
  
  bfun <- function(d, i) {
    dat <- d[i,]
    return((2 * agr(dat)[[1]]) - 1)
  }
  
  byrt <- (2 * agr(d)[[1]]) - 1
  
  bfun_byrt <- function(d,i) {
    dat <- d[i,]
    return((2 * agr(dat)[[1]]) - 1)
  }
  
  b <- boot::boot(d, bfun_byrt, 2000)
  ci <- boot::boot.ci(b, type = 'perc')$percent[4:5]
  
  return(list('po' = agr(d)[[1]], 'byrt' = byrt, 'lb' = ci[1], 'ub' = ci[2]))
}






#-------------------------------------------------------------------------------
#'*--------------------------- PERCENT AGREEMENT -----------------------------'*
#-------------------------------------------------------------------------------

agr <- function(ratings) {
    ratings <- as.matrix(na.omit(ratings))
    ns <- nrow(ratings)
    nr <- ncol(ratings)
    lev <- levels(as.factor(ratings))
    for (i in 1:ns) {
      frow <- factor(ratings[i, ], levels = lev)
      if (i == 1) 
        ttab <- as.numeric(table(frow))
      else ttab <- rbind(ttab, as.numeric(table(frow)))
    }
    ttab <- matrix(ttab, nrow = ns)
    agreeP <- sum((apply(ttab^2, 1, sum) - nr)/(nr * (nr - 1))/ns)
  
  return(list(agreeP, if(exists('ttab')) {ttab}))
}


#-------------------------------------------------------------------------------
#'*------------------------------- MAKE CI -----------------------------------'*
#-------------------------------------------------------------------------------

makeCi <- function(data, bfun, n = 2000) {
  b <- boot::boot(data, bfun, n)
  ci <- boot::boot.ci(b, type = 'perc')$percent[4:5]
  return(ci)
}


#-------------------------------------------------------------------------------
#'*----------------------------- ENTROPY INDEX -------------------------------'*
#-------------------------------------------------------------------------------

doubleEntropy <- function(ratings) {
  
  mydata <- ratings
  n <- max(ratings)
  ticks <- 1:n
  ## n-Number of scales ##
  if(n < 2) {
    print("Error: less than 2 scale values")
    print(mydata)
    
  }
  
  # plot
  Y<-table(mydata)
  
  Y1<-data.frame(Y)
  
  ## m-Number of experts ##
  m<-sum(Y1[,2])
  if(m<2) print("Error: less than 2 experts") 
  
  ## Find frequency distribution ##
  Y1<-data.frame(Y1,Y1[2]/m)
  colnames(Y1)<-c("Score","Freq","Prob")
  
  ## calculating Enropy H2 ##
  H2 <- -(sum(Y1[3]*log(Y1[3])))
  
  ## calculating max Enropy H2 ##
  if(m < n) {maxH2 <- log(m)}  else {
    maxH2 <- (floor(m/n)+1)*log((floor(m/n)+1)/m)*((n/m)*floor(m/n)-1)+
      floor(m/n)*log((floor(m/n))/m)*(1-(n/m)*(floor(m/n)+1))
    }
  
  ## N-Number of scores selected ##
  Y1
  k <- length(Y1[,1])
  
  ## Scores selected ##
  Y2<-as.numeric(as.character(Y1[,1]))
  
  ## calculating  di's - interval on circle ##
  d<-1:k
  for(i in 1:k-1)
  {
    d[i]<-(Y2[i+1]-Y2[i])
  }
  if (k==1) {d[k] <- n} else {
    d[k]<-floor((n-1)/(k-1))+(Y2[1]-1)+(n-Y2[k])
  }
  
  ## calculating probabilities for points on circle ##
  q <- 1:k
  for(i in 1:k)
  {
    q[i] <- d[i]/sum(d)
  }
  
  ## calculating enropy H1 ##
  H1<--sum(q*log(q))
  
  ## calculating max Enropy H1 ##
  if(1 < k) {
    maxH1 <- (1+(k-1)*floor((n-1)/(k-1))-n)*
      log((floor((n-1)/(k-1))+1)/(floor((n-1)/(k-1))+n-1))*
      ((floor((n-1)/(k-1))+1)/(floor((n-1)/(k-1))+n-1))+
    (n-1-k-(k-1)*floor((n-1)/(k-1)))*log((floor((n-1)/(k-1)))/(floor((n-1)/(k-1))+n-1))*
      ((floor((n-1)/(k-1)))/(floor((n-1)/(k-1))+n-1))
  } else {
      maxH1 <- 1
  }
  
  # min H1
  q1 <- 1:k
  for(i in 1:(k-1)) {
    q1[i]<-1/sum(d)
  }
  q1[k] <- (sum(d)-k+1)/sum(d)
  
  ## calculating enropy H1 ##
  minH1 <- -sum(q1*log(q1))
  
  if (maxH1==minH1) {H1n <- 1} else {H1n <- (H1-minH1)/(maxH1-minH1)}
  
  H2n <- H2/maxH2
  
  ## Define R1,R2 ##
  R1 <- round(1- ((n-k)*H1n+k*H2n)/n,4)
  #R2<- round(1 - (H1n*H2n) ,4)
  R1
  #R2
  
  # round(1- H1/(2*maxH1) -H2/(2*maxH2),4)
  
  
  #===============================================================
  
  ## calculating central moments ##
  a <- 1
  moma <- sum(Y1[3]*(abs(Y2-sum(Y2*Y1[3])))^a)
  
  ## maximum of central moments ##
  maxmoma<-(m/(log(m)*n*log(n)))*sum(abs(ticks-(n+1)/2)^a)
  
  ## Define T1, T2 
  z <- 1-as.numeric((min(Y2)==1) & (max(Y2)==n) & (max(abs(Y1[2]-m/k))==0) & (if (k==1) {FALSE} else max(d[-k])==d[1]))

  T1<-(1-(H2+moma)/(maxmoma+maxH2))*z
  T2<-(1-(H2+moma)/((n-1)/2+maxH2))*z
  
  return(R1)
}


#-------------------------------------------------------------------------------
#'*------------------------------ KS VON EYE ---------------------------------'*
#-------------------------------------------------------------------------------

getPropList <- function(inp) {
  vec1 <- colSums(inp)
  vec2 <- rowSums(inp)
  N <- sum(vec2)
  out <- numeric()
  
  for(i in seq_along(vec2)) {
    for(j in seq_along(vec1)) {
      out <- append(out, (vec1[j] * vec2[i] / N))
    }
  }
  return(out)
}

stouffersZ <- function(inp) {
  N <- addmargins(inp)[nrow(addmargins(inp)), ncol(addmargins(inp))]
  N <- N^ncol(inp)
  l <- getPropList(inp)
  l <- t(matrix(l, ncol(inp), ncol(inp)))
  l <- diag(l)
  out <- numeric()
  
  inploop <- diag(inp)
  
  for(i in seq_along(inploop)) {
    if(inploop[i] < .Machine$double.eps^0.5) {
      i <- i + 1
    } else {
      out <- append(out, (inploop[i] - l[i]) / sqrt(l[i] * (1 - (l[i]/N))))
    }
  }
  
  #out <- t(matrix(out, ncol(inp), ncol(inp)))
  
  s <- sum(out)
  
  z <- s * (1 / sqrt(ncol(inp)))
  
  return(z)
}

Ks <- function(ratings) {
  
  if(any(unlist(lapply(ratings, `%%`, 1)) != 0)) stop('not all whole numbers in dataset!')
  
  temp <- function(tabRatings) {
    dat <- addmargins(tabRatings)
    
    oneP <- dat[-ncol(dat), nrow(dat)]
    twoP <- dat[ncol(dat), -nrow(dat)]
    
    props <- (oneP * twoP) / dat[nrow(dat), ncol(dat)]
    
    vals <- numeric()
    for(i in 1:ncol(tabRatings)) {
      nominator <- (dat[i,i] - props[i])
      denominator <- (min(oneP[i], twoP[i]) - props[i])
      
      v <- nominator / denominator
      v[is.nan(v)] <- 0
      
      vals <- append(vals, v)
    }
    
    return(mean(vals))
  }
  
  print((ratings))
  
  if(ncol(ratings) == 2) {
    z <- stouffersZ(table(factor(ratings[,1], min(ratings):max(ratings)), factor(ratings[,2], min(ratings):max(ratings))))
    Ks <- temp(table(factor(ratings[,1], min(ratings):max(ratings)), factor(ratings[,2], min(ratings):max(ratings))))
    return(list('est' = Ks,
                'p.value' = pnorm(z, lower.tail = F)))
  } else {
    stop('more than two raters!')
  }
}


#-------------------------------------------------------------------------------
#'*---------------------------- AD COEFFICIENT -------------------------------'*
#-------------------------------------------------------------------------------

diffFun <- function(vec) {
  return(
    sum(combn(vec, 2, FUN = function(x) {
      return((x[1] - x[2])^2)
    }))
  )
}

adCoeff <- function(ratings, nCat) {
  data <- ratings
  if(ncol(data) == 1) {
    data <- t(data)
  }
  K <- ncol(data)
  J <- nrow(data)
  a <- 1
  b <- nCat
  

  
  d_raw <- if(K == 1) {
    print('K is 1')
    sum(combn(data, 2, function(x) {
      return((x[1] - x[2])^2)
    }))
  } else {
    sum(apply(data, 1, diffFun))
  }
  
  d_max <- if(ncol(data)%%2 == 0) {
    J * (b - a)^2 * (K^2 / 4)
  } else {
    J * (b - a)^2 * ((K^2 - 1) / 4)
  }
  
  return(1 - (d_raw / d_max))
}

adCritical <- function(ratings, nCat) {
  outDist <- numeric()
  m <- mean(unlist(ratings))
  
  x <- rbinom(10000, nCat - 1, ((m - 1) / (nCat - 1)))
  
  for(i in 1:1000) {
    samp <- sample(x, base::prod(dim(ratings)))
    mat <- (matrix(samp, ncol = ncol(ratings), nrow = nrow(ratings))) + 1
    outDist <- append(outDist, adCoeff(mat, nCat))
  }
  
  return(quantile(outDist, 0.95))
}


#-------------------------------------------------------------------------------
#'*------------------------------ A Kappa ------------------------------------'*
#-------------------------------------------------------------------------------



aKappa_slow <- function(ratings) {
  r <- ncol(ratings)
  N <- nrow(ratings)
  k <- max(ratings)
  mat <- agr(ratings)[[2]]
  
  Gis <- numeric()
  below <- (r^2 * (k - 1))
  for(i in seq_len(nrow(mat))) {
    for(j in 1:length(mat[1,])) {
      s <- sum(unlist(lapply(mat[i,], function(x) {
        (x - (r/k))^2
      })))
      Gis <- append(Gis, s/below)
    } 
  }
  
  print(((sum(Gis)/N) - (1/r)) / (1 - (1/r)))
  return(sum(Gis)/N)
}


aKappa <- function(ratings) {
  
  if(0 %in% ratings) {
    ratings <- ratings + 1
  }
  ratings <- as.matrix(ratings)
  r = ncol(ratings)
  N = nrow(ratings)
  k = length(seq(min(ratings), max(ratings), 1))
  mat <- agr(ratings)[[2]]
  below <- (r^2 * (k - 1))
  G <- sum((k * rowSums((mat - (r/k))^2)) / below) / N
  G <- (G - (1/r)) / (1 - (1/r))
  #for readability this stays here
  # mat_ <- (mat - (r/k))^2
  # 
  # mat_s <- rowSums(mat_)
  # 
  # mat_k <- k * mat_s
  # 
  # mat_div <- mat_k / below
  # 
  # G <- sum(mat_div) / N
  
  return(G)
}


#-------------------------------------------------------------------------------
#'*------------------------- FREE RESPONSE KAPPA -----------------------------'*
#-------------------------------------------------------------------------------

freeKappa <- function(ratings) {
  ratings <- table(ratings[,1], ratings[,2])
  
  d <- ratings[4]
  b <- ratings[2]
  c <- ratings[3]
  
  if(is.na(d)) {d <- ratings[1]}
  if(is.na(b)) {b <- 0}
  if(is.na(c)) {c <- 0}
  
  frKap <- (2 * d) / (b + c + (2 * d))
  var <- (b + c + d) / ((b + c) * d)
  return(list('kappa' = frKap))
}

bfun_freeKappa <- function(d, i) {
  dat <- d[i,]
  return(freeKappa(dat)[[1]])
}

#-------------------------------------------------------------------------------
#'*----------------------- INFORMATION AGREEMENT -----------------------------'*
#-------------------------------------------------------------------------------

bmat <- data.frame('a' = c(10, 1, 2),
                   'b' = c(1,8,0),
                   'c' = c(0,2,5))

# props <- addmargins(proportions(as.matrix(bmat)))
# 
# p_x <- props[nrow(props), -ncol(props)]
# p_y <- props[-nrow(props), ncol(props)]
# p_xy <- proportions(as.matrix(bmat))

entropy <- function(vals) {
  return(-sum(vals[vals != 0] * log2(vals[vals != 0])))
}

ia_c <- function(ratings) {
  
  contTable <- as.matrix(table(factor(ratings[,1], min(ratings):max(ratings)),
                               factor(ratings[,2], min(ratings):max(ratings))))
  
  if(ncol(contTable) != nrow(contTable)) {
    stop('matrix has to be square')
  } 
  if(any(contTable < 0)) {
    stop('matrix contains negative values')
  }
  if(all(contTable == 0)) {
    stop('all entries are zero')
  }
  
  p_xy <- proportions(contTable)
  #all_pxy <- addmargins(p_xy)
  p_x <- colSums(p_xy)#all_pxy[nrow(all_pxy), -ncol(all_pxy)]
  p_y <- rowSums(p_xy)#all_pxy[-nrow(all_pxy), ncol(all_pxy)]
  
  h_xf <- entropy(p_x)
  h_yf <- entropy(p_y)
  
  if(h_xf == 0) {
    nonNullRows <- length(rowSums(contTable)[rowSums(contTable) != 0])
    return(nonNullRows / dim(contTable)[1])
  }
  if(h_yf == 0) {
    nonNullCols <- length(colSums(contTable)[colSums(contTable) != 0])
    return(nonNullCols / dim(contTable)[1])
  }
  
  h_xyf = entropy(p_xy)
  
  if(h_xf < h_yf) {
    return( 1 + (h_yf - h_xyf) / h_xf)
  }
  
  return( 1 + (h_xf - h_xyf) / h_yf)
}









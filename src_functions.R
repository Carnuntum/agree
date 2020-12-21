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
    kableExtra::kable(data, format = 'html') %>%
      kableExtra::kable_styling('basic')
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
  
  n_l <- c('chi','odds','pa','pn','kappa')
  dat <- list(xmp_chi, xmp_odds, xmp_pa, xmp_pn, xmp_kappa)
  
  
  set_tabset(output, out_id = 'ui_chi', id = 'id_chi', tableId = 'tab_chi', downId = 'down_chi',
             btnInputId = 'test_chi', data = dat[[1]])

  set_tabset(output, out_id = 'ui_odds', id = 'id_odds', tableId = 'tab_odds', downId = 'down_odds',
             btnInputId = 'test_odds', data = dat[[2]])

  set_tabset(output, out_id = 'ui_pa', id = 'id_pa', tableId = 'tab_pa', downId = 'down_pa',
             btnInputId = 'test_pa', data = xmp_pa)

  set_tabset(output, out_id = 'ui_pn', id = 'id_pn', tableId = 'tab_pn', downId = 'down_pn',
             btnInputId = 'test_pn', data = xmp_pn)

  set_tabset(output, out_id = 'ui_kappa', id = 'id_kappa', tableId = 'tab_kappa', downId = 'down_kappa',
             btnInputId = 'test_kappa', data = xmp_kappa)
  
  set_tabset(output, out_id = 'ui_spear', id = 'id_spear', tableId = 'tab_spear', downId = 'down_spear',
             btnInputId = 'test_spear', data = xmp_spear)
  
  set_tabset(output, out_id = 'ui_polyc1', id = 'id_polyc', tableId = 'tab_polyc1', downId = 'down_polyc1',
             btnInputId = 'test_polyc1', data = xmp_poly1)
  
  make_xmp_table(output, 'tab_polyc2', xmp_poly2)
  
  set_tabset(output, out_id = 'ui_icc', id = 'id_icc', tableId = 'tab_icc', downId = 'down_icc',
             btnInputId = 'test_icc', data = xmp_icc)
  
  set_tabset(output, out_id = 'ui_omega', id = 'id_omega', tableId = 'tab_omega', downId = 'down_omega',
             btnInputId = 'test_omega', data = xmp_omega)
  
  set_tabset(output, out_id = 'ui_kripp', id = 'id_kripp', tableId = 'tab_kripp', downId = 'down_kripp',
             btnInputId = 'test_kripp', data = xmp_kripp)
  
  
  cccTemp <<- round(xmp_ccc[1:10,], 3)
  lapply(cccTemp, as.character)
  cccTemp[[1]][10] = '...'
  cccTemp[[2]][10] = '...'
  
  set_tabset(output, out_id = 'ui_ccc', id = 'id_ccc', tableId = 'tab_ccc', downId = 'down_ccc',
             btnInputId = 'test_ccc', data = cccTemp)
  
  #make_xmp_table(output, 'expccc1', cccTemp)
  
}

#-------------------------------------------------------------------------------
#'*------------------------ SHOW UPLOADED DATA FUN ----------------------------*
#-------------------------------------------------------------------------------

# show_uploaded <- function(output, id, data) {
#   switch(id,
#          chi = output$expChi <- function() {
#            kableExtra::kable(data, format = 'html') %>%
#              kableExtra::kable_styling('basic')
#          },
#          
#          tab_odds = output$expodds1 <- function() {
#            kableExtra::kable(data, format = 'html') %>%
#              kableExtra::kable_styling('basic')
#          },
#          
#          pa = output$expPA <- function() {
#            kableExtra::kable(data, format = 'html') %>%
#              kableExtra::kable_styling(bootstrap_options = 'basic')
#          },
#          
#          pn = output$expPN <- function() {
#            kableExtra::kable(data, format = 'html') %>%
#              kableExtra::kable_styling(bootstrap_options = 'basic')
#          },
#          
#          kappa = output$expKappa <- function() {
#            kableExtra::kable(data, format = 'html') %>%
#              kableExtra::kable_styling('basic')
#          },
#          
#          spear = output$expSpear <- function() {
#            kableExtra::kable(data, format = 'html') %>%
#              kableExtra::kable_styling('basic')
#          },
#          
#          polyc = output$expPolyc1 <- function() {
#            kableExtra::kable(data, format = 'html') %>%
#              kableExtra::kable_styling('basic')
#          },
#          
#          icc = output$expIcc1 <- function() {
#            kableExtra::kable(data, format = 'html') %>% 
#              kableExtra::kable_styling('basic')
#          },
#          
#          omega = output$expOmega1 <- function() {
#            kableExtra::kable(data, format = 'html') %>% 
#              kableExtra::kable_styling('basic')
#          },
#          
#          kripp = output$expKripp1 <- function() {
#            kableExtra::kable(data, format = 'html') %>% 
#              kableExtra::kable_styling('basic', font_size = '10')
#          },
#          
#          ccc = output$expccc1 <- function() {
#            cccTemp <- round(data[1:10,], 3)
#            lapply(cccTemp, as.character)
#            cccTemp[[1]][10] = '...'
#            cccTemp[[2]][10] = '...'
#            kableExtra::kable(cccTemp, format = 'html') %>% 
#              kableExtra::kable_styling('basic', font_size = '12')
#          }
#          
#          )
#   
# }

#-------------------------------------------------------------------------------
#'*------------------------ CHANGE TAB PANEL TITLE ----------------------------*
#-------------------------------------------------------------------------------
# change_tab_title <- function(input, output, id) {
#   switch(id,
#          chi = ,
#          odds = ,
#          pa = ,
#          pn = ,)
# }



#-------------------------------------------------------------------------------
#'*------------------------- DEFAULT OUTPUT FOR ALL ---------------------------*
#-------------------------------------------------------------------------------

defaultOutAll <- function(input, output) {
  

#'*--------------------------------- CHI --------------------------------------*
#-------------------------------------------------------------------------------

  output$chi1 <- renderValueBox({
      valueBox(value = h4('Output'), '')
  })
  
#'*-------------------------------- ODDS --------------------------------------*
#-------------------------------------------------------------------------------

  
  output$odds1 <- renderValueBox({
    if (is.null(input$chiInput)) {
      valueBox(value = h4('Output'), '')
    }
  })
  
#'*-------------------------- PERCENT AGREEMENT -------------------------------*
#-------------------------------------------------------------------------------
  output$pa <- renderValueBox({
    valueBox(value = h4('Output',
                        style = 'text-align: center;
                                  padding: 15px;'), '')
  })
  output$pn <- renderValueBox({
    valueBox(value = h4('Output',
                        style = 'text-align: center;
                                  padding: 15px;'), '')
  })
  
#'*---------------------------------- KAPPA -----------------------------------*
#-------------------------------------------------------------------------------
  output$kappa1 <- renderValueBox({
    valueBox(value = h4(''), subtitle = "Output")
  })
  
#'*------------------------------- ORDINAL RANK -------------------------------*
#-------------------------------------------------------------------------------
  output$ord1 <- renderValueBox({
    valueBox(value = h4("Output",
                        style = 'text-align: center;
                                  padding: 15px;'), '')
  })
  
#'*------------------------------- POLYCHORIC ---------------------------------*
#-------------------------------------------------------------------------------
  output$polyc1 <- renderValueBox({
    valueBox(value = h4("Output",
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

#'*--------------------------------- CCC --------------------------------------*
#-------------------------------------------------------------------------------
  output$ccc1 <- renderValueBox({
    valueBox(value = h4("Output",
                        style = 'text-align: center;
                                padding: 15px;'), '')
  })
  
  
}



#-------------------------------------------------------------------------------
#'*--------------------- DEFAULT OUTPUT FOR ALL END ---------------------------*
#-------------------------------------------------------------------------------

PercentAgreeFor2by2 <- function(userData) {
  
  userData <- as.matrix(userData)
  
  if(nrow(userData) > 2) {
    userData <- as.matrix(table(as.numeric(userData[,1]), as.numeric(userData[,2])))
  } else {
      userData <- as.matrix(userData) %>% addmargins()
      }
  
  if(nrow(userData) == 2 && ncol(userData) == 2) {
    userData <- as.matrix(userData) %>% addmargins()}
  
  PApos <- round((userData[1]/sum(userData[c(1,2,4)]))*100, 3)
  PAneg <- round((userData[5]/sum(userData[c(2,4,5)]))*100, 3)
  
  PAposSF <- round((2*userData[1]/(2*userData[1] + sum(userData[c(2,4)])))*100, 3)
  PAnegSF <- round((2*userData[5]/(2*userData[5] + sum(userData[c(2,4)])))*100, 3)
  
  
  chanceMat <- round(as.matrix(data.frame(((userData[7]*userData[3])/userData[9]),
                                          ((userData[7]*userData[6])/userData[9]),
                                          ((userData[8]*userData[3])/userData[9]),
                                          ((userData[8]*userData[6])/userData[9]))), 3)
  
  PAposExpect <- round((chanceMat[1]/sum(chanceMat[c(1,2,3)]))*100, 3)
  PAnegExpect <- round((chanceMat[4]/sum(chanceMat[c(2,3,4)]))*100, 3)
  
  Kpos <- round((((PApos/100) - (PAposExpect/100)) / (1-(PAposExpect/100))), 3)
  Kneg <- round((((PAneg/100) - (PAnegExpect/100)) / (1-(PAnegExpect/100))), 3)
  
  # m1 <- matrix(c(paste('+',PApos), paste('-',PAneg), PAposExpect, PAnegExpect), ncol = 2)
  # colnames(m1) <- c('proportion','expected')
  # rownames(m1) <- c('positive', 'negative')
  # m2 <- matrix(c(paste('+',PAposSF), paste('-',PAnegSF)))
  # colnames(m2) <- 'conditional proportion'
  # m3 <- matrix(c(paste('+',Kpos), paste('-',Kneg)))
  # colnames(m3) <- 'category specific K'
  
  X2 <- chisq.test(userData[-3,-3], correct = F)
  # X2 <- as(X2, Class = 'list')
  # X2 <- matrix(X2[c(1,2,3)]) %>% 
  #   as.numeric()
  
  X2.corr <- chisq.test(userData[-3,-3], correct = T)
  # X2.corr <- as(X2.corr, Class = 'list')
  # X2.corr <- matrix(X2.corr[c(1,2,3)]) %>% 
  #   as.numeric()
  
  # x2Mat <- round(rbind(X2, X2.corr),4)
  # colnames(x2Mat) <- c('X2','df','p-value')
  
  return(list('paPos' = PApos, 'paNeg' = PAneg, 'paPosSF' = PAposSF,
              'paNegSF' = PAnegSF,'posExpect' = PAposExpect,
              'negExpect' = PAnegExpect, 'kPos' = Kpos, 'kNeg' = Kneg,
              'chiUncor' = X2$statistic, 'chipUncor' = X2$p.value,
              'chidfUncor' = X2$parameter, 'chiValCor' = X2.corr$statistic,
              'chipCor' = X2.corr$p.value, 'chidfCor' = X2.corr$parameter))
}



agreeCorFor2byN <- function(userData) {
  
  userData <- as.data.frame(userData)
  
  wOutNA <- na.omit(userData)
  
  #make factors to create correct contingency 
  a <- factor(wOutNA[,1], levels = 1:max(wOutNA))
  b <- factor(wOutNA[,2], levels = 1:max(wOutNA))
  
  mat <- table(a,b)
  
  chi <- chisq.test(mat)
  
  d <- dim(wOutNA)
  
  mmat <- addmargins(mat)
  
  paTotal <- sum(diag(mat))/d[1]
  
  c <- mmat[1:dim(mmat)[1]-1,colnames(mmat) == 'Sum']
  r <- mmat[rownames(mmat) == 'Sum', 1:dim(mmat)[2]-1]
  
  paExpected <- sum((c * r)/d[1])/d[1]
  
  
  if(sum(is.na(userData)) > 0) {
    nRater1 <- sum(!is.na(userData[,1]))
    nRater2 <- sum(!is.na(userData[,2]))
    
    #if one or both rater have not rated a person
    paWithUncategorizations <- (100 
                                * (2/(nRater1 + nRater2)) 
                                * sum(diag(mat)))/100
    
    uncat <- T
    
    return(list('total' = paTotal, 'expected' = paExpected,
                'uncategorized' = paWithUncategorizations, 'chi' = chi,
                'uncat' = uncat))
  }
  else {
    uncat <- F
    return(list('total' = paTotal, 'expected' = paExpected, 'chi' = chi,
                'uncat' = uncat))
  }
}

text_percAgrPN <- c('uncorrected:  X', '<sup>2</sup>', ' = ',
                         expression(round(as.numeric(test$chiUncor), 3)),
                         ' p = ', expression(round(as.numeric(test$chipUncor), 3)),
                         ' df = ', expression(as.numeric(test$chidfUncor)),
                         '</br>',
                         '</br>',
                         'corrected:  X', '<sup>2</sup>', ' = ',
                         expression(round(as.numeric(test$chiValCor), 3)),
                         ' p = ', expression(round(as.numeric(test$chipCor), 3)),
                         ' df = ', expression(as.numeric(test$chidfCor)))


odds_ <- function(data, alpha = 0.05) {
  
  data <- as.matrix(data)
  
  q <- (data[1] * data[4]) / (data[3] * data[2])
  
  lnQ <- log(q, base = exp(1))
  
  stdErr_lnQ <- sqrt(sum(1/data[1], 1/data[2], 1/data[3], 1/data[4]))
  
  zLN <- lnQ/stdErr_lnQ
  
  pLN <- 2*pnorm(-abs(zLN))
  
  zCrit <- round(qnorm(alpha/2, lower.tail = F), 2)
  
  lnLow <- lnQ - (zCrit * stdErr_lnQ)
  
  lnHigh <- lnQ + (zCrit * stdErr_lnQ)
  
  qLow <- exp(lnLow)
  
  qHigh <- exp(lnHigh)
  
  Y <- (q-1) / (q+1)
  
  stdErr_Y <- 1/4 * (1 - Y^2) * stdErr_lnQ
  
  zY <- Y/stdErr_Y
  
  yLow <- Y - zCrit * stdErr_Y
    
  yHigh <- Y + zCrit * stdErr_Y
  
  transfQLow <- (qLow - 1) / (qLow + 1)
  transfQHigh <- (qHigh -1) / (qHigh + 1)
  
  return(list('oddsRatio' = q,
              'zCrit' = zCrit,
              'logOdds' = lnQ,
              'logStdErr' = stdErr_lnQ,
              'zLog' = zLN,
              'zYule' = zY,
              'yuleY' = Y,
              'pLN' = pLN,
              'logLB' = lnLow,
              'logUB' = lnHigh,
              'oddsLB' = qLow,
              'oddsUB' = qHigh,
              'yuleLB' = yLow,
              'yuleUB' = yHigh,
              'transformedOddsLB' = transfQLow,
              'transformedOddsUB' = transfQHigh))
}

kap_scott_fleiss <- function(data, method) {
  tryCatch({
    if(method == 'kappa') {
      return(list('values' = irr::kappa2(data),
                  'err' = NULL)
             )
    }
    else if(method == 'spi') {
      return(list(
        'values' = rel::spi(data, weight = 'unweighted'),
        'err' = NULL
      ))
    }
    else if(method == 'fleissKappa') {
      return(list(
        'values' = irr::kappam.fleiss(data),
        'err' = NULL
      ))
    }
  }, error = function(e) {
    err <- conditionMessage(e)
    return(list(
      'values' = NULL,
      'err' = err
    ))
  }, warning = function(w) {
    warn <- conditionMessage(w)
    return(list(
      'values' = NULL,
      'err' = warn
    ))
  })
}

#-------------------------------------------------------------------------------
#'*---------------------------- HELPER FUNCTIONS ------------------------------*
#-------------------------------------------------------------------------------
# help_check_ties <- function(data) {
#   return(
#     any(sapply(data, function(x, y) {
#       x <- data[,1]
#       y <- data[,2]
#       x == y
#       }))
#     )
# }
help_check_ties <- function(data) {
  tryCatch({
    return(any(sapply(data, duplicated)))
    
  }, error = function(e) {
    print(e)
  }, warning = function(w) {
    print(w)
  })
}


help_tauInt_sort <- function(data) {
  out <- data.frame()
  k <- na.omit(data)
  
  
  for(i in 1:nrow(k)) {
    m <- min(k)
    w <- which(k == m, arr.ind = T)
    
    out <- rbind(out, k[w[,1][1], ])
    k <- k[-w[,1][1], ]
  }
  return(out)
}

#-------------------------------------------------------------------------------
#'*------------------------ HELPER FUNCTIONS END ------------------------------*
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#'*---------------------------- INTRACLASS TAU --------------------------------*
#-------------------------------------------------------------------------------

intraclassTau <- function(data) {
  nn <- help_tauInt_sort(data)
  nn <- tidyr::pivot_longer(nn,
                     cols = 1:2,
                     names_to = 'new',
                     values_to = 'nums')
  
  idx <- 1
  nums <- nn$nums[3:length(nn$nums)]
  
  above <- 0 
  below <- 0
  for(i in 1:((length(nums)/2))) {
    
    above <- above + sum(sapply(nn$nums[idx], `<`, nums))
    below <- below + sum(sapply(nn$nums[idx], `>`, nums))
    idx <- idx + 1
    above <- above + sum(sapply(nn$nums[idx], `<`, nums))
    below <- below + sum(sapply(nn$nums[idx], `>`, nums))
    
    nums <- nums[-(1:2)]
    
    idx <- idx + 1
  }
  
  S <- above-below
  N <- nrow(data) * ncol(data)
  
  Sp <- S - ((N * (N - 2)) / 4)
  
  sig <- sqrt((N * (N - 2) * (N + 2)) / 18)
  
  u <- (abs(Sp) - 1) / sig
  
  tauIn <- Sp / (N * (N - 2) / 4)
  
  tryCatch({
    p <- if(Sp <= 90 & N <= 20) {
      if(Sp %% 2 == 0) {
        if(is.na(p_table[which(p_table$Sp == Sp), paste0("X", N)])) {
          NULL
        } else {
          as.numeric(p_table[which(p_table$Sp == Sp), paste0("X", N)])
        }
      } else {
        NULL
      }
      
    }
  }, error = function(e) {
    print(paste('following error occured: ', e))
  }, warning = function(w) {
    print(conditionMessage(w))
    
  })
  
  
  return (list('S' = S, 'N' = N,
               'Sp' = Sp, 'sigma' = sig,
               'u' = u, 'tauIn' = tauIn,
               'p.value' = p))
}

#-------------------------------------------------------------------------------
#'*-------------------------------- ODDS  -------------------------------------*
#-------------------------------------------------------------------------------

oddsMain <- function(data, alpha) {
  tryCatch({
    test <- odds_(data, alpha)
    
  }, error = function(e) {
    print(e)
  }, warning = function(w) {
    print(w)
  })
}





#-------------------------------------------------------------------------------
#'*--------------------------- SPEARMAN KENDALL  ------------------------------*
#-------------------------------------------------------------------------------


ordinals <- function(data, method) {
  
  tryCatch ({
    if(method == 'spearman') {
      cor.test(data[,1], data[,2], method = method)
    }
    else if(method == 'kendW') {
      if(help_check_ties(data)) {
        irr::kendall(data, correct = T)
      } else {
        irr::kendall(data)
      }
    }
    #tau A and B are the same B is used in the cor.test function because its
    #already the function which corrects for ties
    else if (method == 'tauB') {
      cor.test(data[,1], data[,2], method = 'kendall')
    }
    else if (method == 'tauC') {
      DescTools::StuartTauC(data[,1], data[,2], conf.level = 0.95)
    }
    else if (method == 'tauIntra') {
      intraclassTau(data)
    }
  },error = function(e) {
    print(e)
    
  })
}

#-------------------------------------------------------------------------------
#'*------------------------------ POLYCHORIC ----------------------------------*
#-------------------------------------------------------------------------------

polyc <- function(data, method) {
  tryCatch({
    
    
    if(method == 'polychoric') {
      return(list('values' = warning_handler(psych::polychoric(data)),
                  'warn' = msg))
    }
    else if (method == 'kruskalG') {
      return(
        list(
          'values' = DescTools::GoodmanKruskalGamma(data, conf.level = 0.95),
          'warn' = NULL))
    }
    else if (method == 'kruskalT') {
      return(
        list(
          'valuesR' = DescTools::GoodmanKruskalTau(data,
                                                   direction = 'row',
                                                   conf.level = 0.95),
          'valuesC' = DescTools::GoodmanKruskalTau(data,
                                                   direction = 'col',
                                                   conf.level = 0.95),
          'warn' = NULL))
    }
    
    
  }, error = function(e) {
    err <- conditionMessage(e)
    return(list(
      'values' = NULL,
      'warn' = err
    ))
    
  }, warning = function(w) {
    warn <- warnings()
    return(
      list(
        'values' = tryCatch({
          psych::polychoric(data)
          }, 
          error = function(e) {
            print(e)
            }
          ),
        'warn' = warn))
    })
}


#-------------------------------------------------------------------------------
#'*--------------------------------- ICC --------------------------------------*
#-------------------------------------------------------------------------------


iccMain <- function(input, output, data, test) {
  
  choices <- c(input$iccChoices, input$iccChoices1, input$iccChoices2)
  
  
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
                                         bootit = 20, parallel = T,
                                         nodes = parallel::detectCores()-1,
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
    
    test <- irr::kripp.alpha(x = data, method = choice)
    
    
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



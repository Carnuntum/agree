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


odds <- function(data, alpha = 0.05) {
  
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
              'logSdtErr' = stdErr_lnQ,
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

ordinals <- function(data, method) {
  
  tryCatch ({
    if(method == 'spearman') {
      cor.test(data[,1], data[,2], method = method)
    }
    else if(method == 'kendall') {
      kendall(data)
    }
  },
  error = function(e) {
    print(e)
  })
}
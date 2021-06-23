d <- data.frame(
  'one' = c(2,2,2,1),
  'two' = c(2,2,2,2),
  'three' = c(3,2,2,2),
  'four' = c(2,2,2,2),
  'five' = c(2,2,1,2)
)

dd <- data.frame(
  'r1' = c(4,8,6,6,1),
  'r2' = c(2,8,3,4,3),
  'r3' = c(1,5,1,2,1),
  'r4' = c(3,7,5,1,2)
)

conger_dat <- data.frame(
  r1 = c(1,2,3,3,2,1,4,1,2,NA,NA,NA),
  r2 = c(1,2,3,3,2,2,4,1,2,5,NA,NA),
  r3 = c(NA,3,3,3,2,3,4,2,2,5,1,3),
  r4 = c(1,2,3,3,2,4,4,1,2,5,1,NA)
)

ccc_pairwise <- function(frame) {
  s <- apply(combn(nrow(frame), 2), 2, function(x) return(CCC(frame[x[1],], frame[x[2],], ci = 'z-transform')))
  return(s)
}

sol2 <- function(frame) {
  s <- combn(nrow(frame), 2, FUN = function(x) {return(CCC(frame[x[1],], frame[x[2],]))}, simplify = F)
  return(s)
}

# o_ccc <- function(frame) {
#   #after barnhart (2002) p 2
#   
#   terms <- as.data.frame(combn(ncol(frame), 2, FUN = function(x) {
#   
#     cov_term <- 2 * var(frame[,x[1]], frame[,x[2]])
#     div_term <- (mean(frame[,x[2]]) - mean(frame[,x[1]]))^2
#     
#     return(list('v' = cov_term, 'w' = div_term))
#     
#   }, simplify = T))
#   
#   terms <- lapply(terms, as.numeric)
#   terms <- as.data.frame(terms)
#   
#   cov <- sum(terms[1,])
#   div <- sum(terms[2,])
#   
#   # div_term <- sum(as.numeric(combn(ncol(frame), 2, FUN = function(x) {return(
#   #   (mean(frame[,x[2]]) - mean(frame[,x[1]]))^2
#   # )}, simplify = F)))
#   # 
#   var_term <- (ncol(frame) - 1) * sum(as.numeric(lapply(frame, var)))
#   # 
#   # out <- cov_term / (div_term + var_term)
#   
#   
#   return(cov / (div + var_term))
# }
# 
# l <- numeric()
# for(i in 1:1000) {
#   
#   t1 <- Sys.time()
#   o_ccc(dd)
#   l[i] <- Sys.time() - t1
#   
# }
# print(paste("time: ", mean(l)))


o_ccc <- function(frame) {
  
  terms <- combn(ncol(frame), 2, FUN = function(x) {
    
    first <- frame[,x[1]]
    second <- frame[,x[2]]
    
    scale_shift <- sd(first)/sd(second)
    
    location_shift <- (mean(first) - mean(second)) / 
      sqrt(sd(first) * sd(second))
    
    cov_term <- 2 * var(first, second)
    
    ksi_term <- (mean(first) - mean(second))^2 + var(first) + var(second)
    
    ccc <- cov_term / ksi_term
    
    #standard error for ccc
    rc <- fisherz(ccc)
    r <- cor(first, second)
    n_term <- (1 / (nrow(frame) - 2))
    se <- sqrt(
      n_term *
        (
          ((1 - r^2) * ccc^2) / ((1 - ccc^2) * r^2) +
          (2 * ccc^3 * (1 - ccc) * u^2) / (r * (1 - ccc^2)^2) -
          (ccc^4 * u^4) / (2 * r^2 * (1 - ccc^2)^2)
        )
    )
    
    lower <- rc - 1.96 * se
    upper <- rc + 1.96 * se
    lower <- fisherz2r(lower)
    upper <- fisherz2r(upper)
    
    return(data.frame('cov' = unlist(cov_term), 'ksi' = unlist(ksi_term),
                      'ccc' = unlist(ccc),
                      'scale' = unlist(scale_shift),
                      'location' = unlist(location_shift),
                      'lower' = unlist(lower),
                      'upper' = unlist(upper),
                      'se' = se,
                      'r' = r
                      ))
  } ,simplify = F)
  
  dOut <- data.frame(terms[[1]])
  for(i in 2:length(terms)) {
    dOut <- rbind(dOut, terms[[i]])
  }
  
  #accuraccy vector
  dOut$accu <- ((dOut$scale + (1/dOut$scale) + (dOut$location)^2) / 2)^-1
  
  #overall accuracy
  oaccu <- sum(dOut$ksi * dOut$accu) / sum(dOut$ksi)
  
  #overall ccc
  occc <- sum(dOut$ksi * dOut$ccc) / sum(dOut$ksi)
  
  #overall precision
  oprec <- occc/oaccu
  
  #overall scale and location shift
  oscale <- sum(dOut$ksi * dOut$scale) / sum(dOut$ksi)
  oloc <- sum(dOut$ksi * dOut$location) / sum(dOut$ksi)
  
  # olower <- sum(dOut$ksi * dOut$lower) / sum(dOut$ksi)
  # oupper <- sum(dOut$ksi * dOut$upper) / sum(dOut$ksi)
  olower <- mean(dOut$lower)
  oupper <- mean(dOut$upper)
  
  
  #questionable
  SE <- sd(dOut$ccc) / sqrt(length(dOut$ccc))
  N <- nrow(frame)

  if(nrow(frame) <= 25) {
    lw.ci <- occc - 1.96 * (N / (N - 3)) * SE
    up.ci <- occc + 1.96 * (N / (N - 3)) * SE
  } else if(nrow(frame) <= 50) {
    lw.ci <- occc - 1.96 * (N / (N - 2)) * SE
    up.ci <- occc + 1.96 * (N / (N - 2)) * SE
  } else {
    lw.ci <- occc - 1.96 * (N / (N - 1)) * SE
    up.ci <- occc + 1.96 * (N / (N - 1)) * SE
  }
  
  return(list('mat' = dOut, 'occc' = occc, 'oaccu' = oaccu, 'oprec' = oprec,
              'l.ci' = lw.ci,
              'u.ci' = up.ci,
              olower = olower,
              oupper = oupper,
              'oscale' = oscale, 
              'oloc' = oloc))
}


l <- numeric()
for(i in 1:1000) {
  
  t1 <- Sys.time()
  o_ccc_better(dd)
  l[i] <- Sys.time() - t1
  
}
print(paste("time: ", mean(l)))






# divideTermTest <- function(frame) {
#     s <- combn(ncol(frame), 2, FUN = function(x) {return(
#       (mean(frame[,x[2]]) - mean(frame[,x[1]]))^2
#     )}, simplify = F)
#     return(sum(as.numeric(s)))
# }
# 
# covTerm <- function(frame) {
#   s <- combn(ncol(frame), 2, FUN = function(x) {return(
#     2 * var(frame[,x[1]], frame[,x[2]])
#   )})
#   return(sum(as.numeric(s)))
# }
# 
# l1 <- list()
# l2 <- list()
# l3 <- list()
# 
# for(i in 1:1000) {
#   t1 <- Sys.time()
#   ccc_pairwise(t(dd))
#   time1 <- Sys.time() - t1
#   
#   t1 <- Sys.time()
#   sol2(t(dd))
#   time2 <- Sys.time() - t1
#   
#   t1 <- Sys.time()
#   epi.occc(dd, pairs = T)
#   time3 <- Sys.time() - t1
#   
#   
#   
#   l1[i] <- time1
#   l2[i] <- time2
#   l3[i] <- time3
# }
# 
# 
# 
# cat(paste("first time: ", mean(as.numeric(l1)), "\nsecond time: ", mean(as.numeric(l2)),
#           "\nthird time: ", mean(as.numeric(l3))))



# o <- apply(combn(length(dd[,1]), 2), 2, function(x) {
#   a <- data.frame(dd[x[1],], dd[x[2],])
#   colnames(a) <- c("r1", "r2")
#   return(as.numeric(agreeCorFor2byN(a)$total))
# })

l1 <- numeric()
l2 <- numeric()

for(i in 1:1000) {
  t1 <- Sys.time()
  agr(d)
  l1[i] <- Sys.time() - t1
  
  t1 <- Sys.time()
  suppressWarnings(pairwiseAgree(d))
  l2[i] <- Sys.time() - t1
}
mean(l1)
mean(l2)

#'*-------------------------- new kappa s von eye 2006 --------------------- *'#
#table 1. data in paper
a <- c(rep(1, 61), rep(1, 15),1,rep(1, 12),rep(1,7),rep(1,5),1,rep(1,4),rep(1,3),rep(2,10),rep(2,10),2,rep(2,14),rep(2,36),rep(2,16),rep(2,5),rep(2,18),rep(2,25),3,rep(3,7),rep(3,7),3,3,rep(3,18),rep(3,18),rep(3,18),rep(3,105))
b <- c(rep(1, 61), rep(1, 15),1,rep(2,12),rep(2,7),rep(2,5),3,rep(3,4),rep(3,3),rep(1,10),rep(1,10),1,rep(2,14),rep(2,36),rep(2,16),rep(3,5),rep(3,18),rep(3,25),1,rep(1,7),rep(1,7),2,2,rep(2,18),rep(2,18),rep(3,18),rep(3,105))                                                                      
c <- c(rep(1, 61), rep(2, 15),3,rep(1,12),rep(2,7),rep(3,5),1,rep(2,4),rep(3,3),rep(1,10),rep(2,10),3,rep(1,14),rep(2,36),rep(3,16),rep(1,5),rep(2,18),rep(3,25),1,rep(2,7),rep(3,7),1,1,rep(2,18),rep(3,18),rep(2,18),rep(3,105))

a <- c(rep(1,9),1,rep(2,12),rep(2,14))
b <- c(rep(1,9),2,rep(1,12),rep(2,14))

a <- c(rep(1,80),rep(1,36),rep(1,10),rep(2,30),rep(2,67),rep(2,41),rep(2,2),rep(3,6),rep(3,41),rep(3,85),rep(3,17),rep(4,4),rep(4,25),rep(4,21))
b <- c(rep(1,80),rep(2,36),rep(3,10),rep(1,30),rep(2,67),rep(3,41),rep(4,2),rep(1,6),rep(2,41),rep(3,85),rep(4,17),rep(2,4),rep(3,25),rep(4,21))

dat <- matrix(c(9,12,1,14), nrow = 2, ncol = 2)
data(landis) #magree

landis <- as.data.frame(landis)
lan <- data.frame(a = e, b = f)

freqs <- numeric()
for(i in 1:nrow(landis)) {
  if(isTRUE(diff(range(landis[i,])) < .Machine$double.eps ^0.5)) {
    freqs <- append(freqs, landis[i,1])
  }
}

table(freqs)

Ks <- function(ratings) {
  temp <- function(tabRatings) {
    dat <- addmargins(tabRatings)
    
    oneP <- dat[-ncol(dat), nrow(dat)]
    twoP <- dat[ncol(dat), -nrow(dat)]
    
    props <- (oneP * twoP) / dat[nrow(dat), ncol(dat)]
    
    vals <- numeric()
    for(i in 1:ncol(tabRatings)) {
      v <- (dat[i,i] - props[i]) / (min(oneP[i], twoP[i]) - props[i])
      vals <- append(vals, v)
    }
    
    return(mean(vals))
  }
  
  if(ncol(ratings) == 2) {
    return(temp(table(ratings)))
  } else {
    out <- combn(ratings, 2, FUN = function(x) {
      return(temp(table(x)))
    })
    return(mean(out))
  }
}

erg <- combn(d, 2, FUN = function(x) {
  print(Ks(table(x)))
})


#'*---------------------- new kappa s von eye 2006 END --------------------- *'#

#using magree landis data
pe <- numeric(0)
conv_mat <- agr(landis)[[2]]
M <- max(landis)
ni <- ncol(landis)
pairs <- numeric()

combFun <- function(pair) {
  wjk <- 1 - (abs(pair[1] - pair[2]) / (M - 1))
  nij_nik <- pair[1] * pair[2]
  return(wjk)
}

for(i in seq_len(nrow(landis))) {
  row <- landis[i,]
  pi <- sum(combn(row, 2, combFun)) / (ni*(ni -1))
  pe[i] <- pi
}

out1 <- numeric()
for(j in 1:4) {
  for(k in j+1:5) {
    wjk <- 1 - ((j - k) / 4)
    print(wjk)
  }
}

#'*--------------- PROBABLY FUNCTIONING S COEFFICIENT 2014 ------------------*'#

above <- numeric()
out <- numeric()

for(i in 1:118) {
  for(j in 1:5) {
    for(k in 1:5) {
      wjk <- 1 - (abs(j - k) / (M - 1))
      prod <- wjk * (conv_mat[i,j] * conv_mat[i,k])
      above <- append(above, prod)
    }
  }
  above <- sum(above) - ni
  above <- above / (ni * (ni - 1))
  out <- append(out, above)
  above <- numeric()
}
mean(out)

weights <- numeric()
for(j in 1:5) {
  for(k in 1:5) {
    weights <- append(weights, (1 - ((abs(j - k))/(M - 1))))
  }
}
Po <- mean(out)
Pe <- (1/(M^2)) * sum(weights)
print((Po - Pe) / (1 - Pe))

sCoefficient <- function(ratings, categories) {
  conv_mat <- agr(ratings)[[2]]
  M <- categories
  ni <- dim(ratings)[2]
  rows <- dim(ratings)[1]
  above <- numeric()
  out <- numeric()
  
  for(i in seq_len(rows)) {
    for(j in seq_len(M)) {
      for(k in seq_len(M)) {
        wjk <- 1 - (abs(j - k) / (M - 1))
        prod <- wjk * (conv_mat[i,j] * conv_mat[i,k])
        above <- append(above, prod)
      }
    }
    above <- sum(above) - ni
    above <- above / (ni * (ni - 1))
    out <- append(out, above)
    above <- numeric()
  }
  
  weights <- numeric()
  for(j in 1:M) {
    for(k in 1:M) {
      weights <- append(weights, (1 - ((abs(j - k))/(M - 1))))
    }
  }
  Po <- mean(out)
  Pe <- (1/(M^2)) * sum(weights)
  s <- ((Po - Pe) / (1 - Pe))
  return(s)
}

sCoefficientCPP <- function(ratings, categories) {
  conv_mat <- agr(ratings)[[2]]
  M <- categories
  ni <- dim(ratings)[2]
  rows <- dim(ratings)[1]
  above <- numeric()
  loopInput <- list()
  
  for(i in 1:dim(conv_mat)[1]) {
    loopInput[[i]] <- conv_mat[i, ]
  }
  
  out <- loopMain(loopInput, M, ni);
  
  # for(i in seq_len(rows)) {
  #   for(j in seq_len(M)) {
  #     for(k in seq_len(M)) {
  #       wjk <- 1 - (abs(j - k) / (M - 1))
  #       prod <- wjk * (conv_mat[i,j] * conv_mat[i,k])
  #       above <- append(above, prod)
  #     }
  #   }
  #   above <- sum(above) - ni
  #   above <- above / (ni * (ni - 1))
  #   out <- append(out, above)
  #   above <- numeric()
  # }
  
  # weights <- numeric()
  # for(j in 1:M) {
  #   for(k in 1:M) {
  #     weights <- append(weights, (1 - ((abs(j - k))/(M - 1))))
  #   }
  # }
  
  weights <- loopWeights(M);
  
  Po <- mean(out)
  Pe <- (1/(M^2)) * sum(weights)
  s <- ((Po - Pe) / (1 - Pe))
  return(s)
}

t1 <- Sys.time()
for(i in 1:1000) {
  a <- sCoefficientCPP(landis, 5)
}
Sys.time() - t1

t1 <- Sys.time()
for(i in 1:1000) {
  a <- irrCAC::bp.coeff.raw(landis, weights = 'linear')
}
Sys.time() - t1

#rcpp version

Rcpp::cppFunction('
            std::vector<double> loopWeights(double M) {
              std::vector<double> weights{};
              for(int j = 0; j < (int)M; ++j) {
                for(int k = 0; k < (int)M; ++k) {
                  weights.push_back(1 - (std::abs(j - k) / (M - 1)));
                }
              }
              return(weights);
            }')

Rcpp::cppFunction('
          std::vector<double> loopMain(std::vector<std::vector<double>> conv_mat, double M, double ni) {
          std::vector<double> above{};
          std::vector<double> out{};
          double wjk = 0.;
          double prod = 0.;
          double abv_temp = 0.;
          const double ni_const = (ni * (ni - 1));
          
          auto sum = [&](std::vector<double> input) {
            auto out = 0.;
            for(int i = 0; i < (int)input.size(); ++i) {
              out += input.at(i);
            }
            return(out);
          };
          
          for(int i = 0; i < (int)conv_mat.size(); ++i) {
            for(int j = 0; j < (int)M; ++j) {
              for(int k = 0; k < (int)M; ++k) {
                wjk = 1 - (std::abs(j - k) / (M - 1));
                prod = wjk * (conv_mat.at(i).at(j) * conv_mat.at(i).at(k));
                above.push_back(prod);
              }
            }
            abv_temp = sum(above) - ni;
            abv_temp = abv_temp / ni_const;
            out.push_back(abv_temp);
            abv_temp = 0.;
            above.clear();
          }
          return(out);
        }')

#'*--------------- PROBABLY FUNCTIONING S COEFFICIENT 2014 ------------------*'#

x <- 2
w <- numeric()
wnn <- numeric()
n_s <- numeric()

for(j in 1:4) {
  for(k in x:5) {
    wjk <- (1 - (abs(j - k) / 4))
    wnn <- append(wnn, wjk * xx[j] * xx[k])
  }
  x <- x + 1
}

for(i in 1:5) {
  n_s[i] <- xx[i]^2x
}



######################## HUBERT multirater ###################################
#from landis data
library(magree)
data(landis)

#for table 1a. in paper
lan3 <- as.data.frame(landis[,1:3])
#count agreement frequencies
lan3 <- as.data.frame(table(lan3))
lan3 <- as.data.frame(lapply(lan3, as.numeric))

#loop over frequencies and find agreeing frequencies e.g. 111, 222, 333 etc.
#to get the values for calculating raw agreement
lan4 <- lan3[,-4]
out <- numeric()
for(i in 1:nrow(lan4)) {
  if(isTRUE(diff(range(lan4[i,])) < .Machine$double.eps ^0.5)) {
    out <- append(out, (lan3[i,]$Freq))
  }
}


#lan3 <- as.data.frame(lapply(lan3, factor, levels = 1:5))


#for table 1b. in paper - loop over input data to get overall frequencies for
#each rater for each category
for(i in 1:ncol(landis)) {
  if(i == 1) {
    c <- landis %>% count(.[,i])
    n <- c[,2]
    out <- data.frame(n)
    print(out)
  } else {
    c <- landis %>% count(.[,i])
    n <- c[,2]
    out <- cbind(out, n)
  }
}
out


#raw data table4 von eye 2006 Ks coefficient
e <- c(rep(1,11), 1, 1, rep(1,19), 2, 2,2,2, rep(2,3), rep(3,8), rep(3,82))
f <- c(rep(1,11), 2, 2, rep(3,19), 1, 2,2,2, rep(3,3), rep(2,8), rep(3,82))

getProps <- function(inp) {
  inp <- addmargins(inp)
  N <- inp[nrow(inp), ncol(inp)]
  x <- inp[nrow(inp),][-ncol(inp)]
  y <- inp[,ncol(inp)][-nrow(inp)]
  
  out <- numeric()
  
  for(i in seq_along(y)) {
    for(j in seq_along(x)) {
      out <- append(out, (y[i] * x[j] / N))
    }
  }
  return(out)
}

stouffersZ <- function(inp) {
  N <- addmargins(inp)[nrow(addmargins(inp)), ncol(addmargins(inp))]
  N <- N^ncol(inp)
  l <- getPropList(inp)
  l <- t(matrix(l, ncol(inp), ncol(inp)))
  out <- numeric()
  
  for(i in seq_along(inp)) {
      out <- append(out, (inp[i] - l[i]) / sqrt(l[i] * (1 - (l[i]/N))))
  }
  
  print(out)
  
  out <- t(matrix(out, ncol(inp), ncol(inp)))
  
  s <- numeric()
  for(i in 1:ncol(inp)) {
    s <- append(s, out[i,i])
  }
  
  z <- sum(s) * (1 / sqrt(ncol(inp)))
  
  return(z)
}

#von eye Ks for 2 raters and 2 categories
Ks <- function(inp) {
  props <- getProps(addmargins(inp))
  marge <- addmargins(inp)
  colmarge <- marge[,ncol(marge)][-ncol(marge)]
  rowmarge <- marge[nrow(marge),][-nrow(marge)]
  first <- props[1]
  second <- props[4]
  
  out <- 0.5 * (((inp[1,1] - first) / (min(colmarge) - first)) + 
    ((inp[2,2] - second) / (min(rowmarge) - second)))
  
  return(out)
}


#get all cohen type proportions for 2x2 table
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



#raw agreement
agr <- function(ratings) {
  if(ncol(ratings) == nrow(ratings)) {
    agreeP <- sum(diag(ratings))/sum(colSums(ratings))
  } else {
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
  }
  return(list(agreeP, ttab))
}




#'*----------------------------HUBERTS KAPPA ------------------------------- *'#

HUBERT_KAPPA <- function(ratings) {
  ratings <- na.omit(ratings)
  out <- list()
  N <- dim(ratings)[1]
  nRaters <- dim(ratings)[2]
  
  #find just cases when all raters agree
  for(i in 1:N) {
    if(var(unlist(ratings[i,])) < .Machine$double.eps ^0.5) {
      out <- append(out, list(ratings[i,]))
    }
  }
  
  #convert list to dataframe
  df <- data.frame(matrix(unlist(out), nrow=length(out), byrow=TRUE))
  df <- as.data.frame(lapply(df, as.numeric))
  
  #count agreeing cases
  df <- df %>% group_by(.[]) %>% count()
  agreeVals <- df[,ncol(df)]
  
  totalPerCategory <- list()
  for(i in 1:nRaters) {
    l <- unlist((ratings %>% group_by(.[,i]) %>% count)[,2])
    totalPerCategory <- append(totalPerCategory, list(l))
  }
  totalPerCategoryDF <- data.frame(matrix(unlist(totalPerCategory),
                                          nrow = length(totalPerCategory),
                                          byrow = TRUE))
  
  Po <- sum(agreeVals)/N
  Pe <- sum(apply(totalPerCategoryDF, 2, base::prod))/N^nRaters
  return((Po - Pe) / (1 - Pe))
}


lan_33 <- lan3[,-4]
lanCopy <- lan3
sums <- list()
for(i in 1:dim(lan_33)[1]) {
  sums <- append(sums, sum(combn(unlist(lan_33[i,]), 2, FUN = function(x) {1 - (abs(x[1] - x[2])) / (3-1) })))
}
sums <- unlist(sums)

w <- numeric()
temp <- numeric()
for(k in 1:27) {
  for(i in 1:3) {
    for(j in i+1:3) {
      temp <- append(temp, abs(lan_33[k, i] - lan_33[k, j]))
      
    }
  }
  
  w <- append(w, sum(temp))
  temp <- numeric()
}




bfun_bp <- function(d, i) {
  dat <- d[i,]
  return(bp(dat, print = F))
}


#'*-------------------------- OWN CI Replicate ------------------------------*'#

sampling <- function(data) {
  out <- as.data.frame(lapply(data, FUN = function(x) {
    sample(x, length(x), replace = T)
  }))
  return(out)
}

output <- function(n, data) as.data.frame(replicate(n, sampling(data = data)))


bfun <- function(d) {
  return(KappaM(d)[1])
}

ownBoot <- function(n, data) {
  
  ddl <- lapply(output(n, data), as.data.frame)
  return(lapply(ddl, bfun))
}



####################### van oest kappa ########################################
agr <- function(ratings) {
  ratings <- as.matrix(na.omit(ratings))
  ns <- nrow(ratings)
  nr <- ncol(ratings)
  lev <- levels(as.factor(ratings))
  
  
  # itemsByCategory <- t(apply(ratings, 1, FUN = function(row) {
  #   as.numeric(table(factor(row, lev)))
  # }))
  
  ratersByCategory <- t(apply(ratings, 2, FUN = function(row) {
    as.numeric(table(factor(row, lev)))
  }))
  
  # itemsByCategory <- matrix(itemsByCategory, nrow = ns)
  # ratersByCategory <- matrix(ratersByCategory, nrow = nr)
  agreeP <- pa(ratings)
  
  return(list('Pa' = agreeP$pa, 'itemsByCat' = agreeP$agreeMat, 'ratersByCat' = ratersByCategory))
}

rm(a)
t1 <- Sys.time()
for(i in 1:1000) {
  a <- agr(landis)
}
print(Sys.time() - t1)



data(landis)
test <- data.frame(landis[,1], landis[,2])

# mat <- agr_col(test)[[2]]
# 
# cols <- colSums(mat)
# cols <- cols / (2 * 118)
# cols <- cols^2
# sum(cols)

Ir <- function(ratings) {
  all  <- agr_col(ratings)
  Pa <- all[[1]]
  k <- ncol(all[[2]])
  mat <- all[[3]]
  r <- nrow(mat)
  N <- nrow(ratings)
  
  above <- 1 + colSums(mat)
  below <- k + (r * N)
  
  Pe <- sum((above / below)^2)
  return((Pa - Pe) / (1 - Pe))
}

t1 <- Sys.time()
for(i in 1:1000) {
  a <- Ir(r)
}
print(Sys.time() - t1)

bfun_Ir <- function(d, i) {
  dat <- d[i,]
  return(Ir(dat))
}

b <- boot::boot(ratings, bfun_Ir, 2000)


pa <- function (ratings) {
  ns <- nrow(ratings)
  nr <- ncol(ratings)
  ratings.mat <- as.matrix(ratings)
  n <- nrow(ratings.mat)
  r <- ncol(ratings.mat)
  categ <- sort(unique(na.omit(as.vector(ratings.mat))))
  q <- length(categ)
  
  agree.mat <- matrix(0, nrow = n, ncol = q)
  for (k in 1:q) {
    categ.is.k <- (ratings.mat == categ[k])
    agree.mat[, k] <- (replace(categ.is.k, is.na(categ.is.k), FALSE)) %*% rep(1, r)
  }
  pa <- sum((rowSums(agree.mat^2) - nr)/(nr * (nr - 1))/ns)
  
  return(list('pa' = pa, 'agreeMat' = agree.mat))
}





x1 <- rbinom(100, 1, 0.8)
x2 <- rbinom(100, 1, 0.8)

y1 <- rbinom(100, 1, 0.2)
y2 <- rbinom(100, 1, 0.2)

bin1 <- as.matrix(data.frame(x1, x2))
bin2 <- as.matrix(data.frame(y1, y2))
hist(bin1)
hist(bin2)

addmargins(table(as.data.frame(bin1)))
addmargins(table(as.data.frame(bin2)))




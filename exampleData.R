chiTableExp <- data.frame(athletes = c(273,266),
                          students = c(10218,14081),
                          row.names = c('male','female'))


exactTest <- function(data) {
  
  for(i in 1:nrow(data)) {
    for(j in 1:ncol(data)) {
      if(data[i,j] <= 5) return(T)
    }
  }
  return(F)
}

percAgrTableExp <- matrix(data = c(c(2,2,3,3,4,6,8),
                                   c(2,3,4,3,4,6,8)), ncol = 2,
                          dimnames = list(c(seq(1, 7)),
                                          c('rater1','rater2')))

percAgrPosNeg <- matrix(data = c(c(1,1,0,0,1,1,0,1,0),
                                 c(1,0,0,0,1,1,1,1,0)), ncol = 2,
                        dimnames = list(c(seq(1,9)),
                                        c('rater1', 'rater2')))

kappaTableExample <- matrix(data = c(c(2,2,3,3,4,6,8,1,5,7),
                                     c(2,3,4,3,4,6,8,1,5,7)), ncol = 2,
                            dimnames = list(c(seq(1,10)),
                                            c('rater1','rater2')))

spearmanTableExample <- data.frame('rater1' = c(1,7,6,5,5,2,9),
                                   'rater2' = c(3,9,6,6,7,4,8))

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

kendTauWOutTies <- data.frame('rater1' = c(1,2,3,4,5),
                             'rater2' = c(2,1,4,5,3))

kendTauWithTies <- data.frame('rater1' = c(1,2,2,4,4,6,6,8,9,9),
                              'rater2' = c(1,2,4,4,4,4,8,8,8,10))

kendTauC <- data.frame('rater1' = c(15,2,16,7,4,13,6,8),
                       'rater2' = c(12,1,10,11,3,14,5,9))

p_table <- read.csv2('example_data/tauIntra_p_table.csv')

polycXmp <- as.table(matrix(c(23,45,23,67,45,78,45,12,78), 3, 3))

polycXmp2 <- round(data.frame('rater1' = rnorm(7, 3, 1),
                              'rater2' = rnorm(7,3, 1),
                              'rater3' = rnorm(7, 3, 1)))
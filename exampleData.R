

xmp_odds <- matrix(data = c(c(19,10),
                           c(8,21)),
                  ncol = 2,
                  dimnames = list(c(1,0), c(1,0)))


xmp_pa <- matrix(data = c(c(2,2,3,3,4,6,8),
                                   c(2,3,4,3,4,6,8)), ncol = 2,
                          dimnames = list(c(seq(1, 7)),
                                          c('rater1','rater2')))

xmp_pn <- matrix(data = c(c(1,1,0,0,1,1,0,1,0),
                                 c(1,0,0,0,1,1,1,1,0)), ncol = 2,
                        dimnames = list(c(seq(1,9)),
                                        c('rater1', 'rater2')))

xmp_kappa <- matrix(data = c(c(2,2,3,3,4,6,8,1,5,7),
                                     c(2,3,4,3,4,6,8,1,5,7)), ncol = 2,
                            dimnames = list(c(seq(1,10)),
                                            c('rater1','rater2')))

xmp_spear <- data.frame('rater1' = c(1,7,6,5,5,2,9),
                                   'rater2' = c(3,9,6,6,7,4,8))

xmp_tauw <- data.frame('rater1' = c(1,2,3,4,5),
                             'rater2' = c(2,1,4,5,3))

xmp_tauTies <- data.frame('rater1' = c(1,2,2,4,4,6,6,8,9,9),
                              'rater2' = c(1,2,4,4,4,4,8,8,8,10))

xmp_tauc <- data.frame('rater1' = c(15,2,16,7,4,13,6,8),
                       'rater2' = c(12,1,10,11,3,14,5,9))

p_table <- read.csv2('example_data/tauIntra_p_table.csv')

xmp_poly1 <- as.table(matrix(c(23,45,23,67,45,78,45,12,78), 3, 3))

xmp_poly2 <- round(abs(data.frame('rater1' = c(3,3,3,3,3,3,4),
                              'rater2' = c(2,3,3,4,3,3,3),
                              'rater3' = c(3,3,3,3,3,2,3))))

xmp_icc <- round(abs(data.frame('rater1' = rnorm(6, 4, 1.5),
                               'rater2' = rnorm(6, 4, 1.5),
                               'rater3' = rnorm(6, 4, 1.5))))

xmp_omega <- matrix(c(1,2,3,3,2,1,4,1,2,NA,NA,NA,
                     1,2,3,3,2,2,4,1,2,5,NA,3,
                     NA,3,3,3,2,3,4,2,2,5,1,NA,
                     1,2,3,3,2,4,4,1,2,5,1,NA), 12, 4)
colnames(xmp_omega) = c("c.1.1", "c.2.1", "c.3.1", "c.4.1")

xmp_kripp <- t(matrix(c(1,1,NA,1,2,2,3,2,3,3,3,3,3,3,3,3,2,2,2,2,1,2,3,4,4,4,4,4,
                     1,1,2,1,2,2,2,2,NA,5,5,5,NA,NA,1,1,NA,NA,3,NA),nrow=4))

colnames(xmp_kripp) <- paste0('r', 1:4)

set.seed(seed = 1234)
xmp_ccc <- data.frame('m1' = as.numeric(rnorm(n = 100, mean = 0, sd = 1)),
                     'm2' = as.numeric(rnorm(n = 100, mean = 0, sd = 1) + 
                               runif(n = 100, min = 0, max = 1)))


# l <- list(xmp_chi, xmp_odds, xmp_pa, xmp_pn, xmp_kappa, xmp_spear,
#       xmp_poly1, xmp_poly2, xmp_icc, xmp_omega, xmp_kripp, xmp_ccc)
# 
# xmp_l <<- list()
# for(i in 1:12) {
#   xmp_l[[paste0(i)]] <- l[[i]]
# }


xmp_rwg <- data.frame('group' = c(1,1,1))
xmp_rwg <- cbind(xmp_rwg, t(xmp_poly2)[,1:5])
colnames(xmp_rwg) <- c('group', paste0('item', seq(1, (ncol(xmp_rwg) - 1))))

library(Rcpp)
library(RcppArmadillo)
Rcpp::sourceCpp("src/kmeans.cpp")


mat <- matrix(c(1,2,3,4,5), 5, 5)


matrix(rowSums(mat^2), nrow = 5, ncol = 5)

testing(mat)

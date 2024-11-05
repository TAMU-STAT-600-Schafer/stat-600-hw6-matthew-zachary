library(Rcpp)
library(RcppArmadillo)
Rcpp::sourceCpp("src/kmeans.cpp")
#' Title
#'
#' @param X n by p matrix containing n data points to cluster
#' @param K integer specifying number of clusters
#' @param M optional, K by p matrix of cluster centers
#' @param numIter number of maximal iterations for the algorithm, the default value is 100
#'
#' @return Return the vector of assignments Y
#' @export
#'
#' @examples
#' # Give example
#' set.seed(123)
#' #constructing toy data 
#' X_first_half = rnorm(50, 0, 1)
#' X_second_half = rnorm(50, 100, 2)
#' X_2_first_half = rnorm(50, 5, 2)
#' X_2_second_half = rnorm(50, 95, 2)
#' X_1 = c(X_first_half, X_second_half)
#' X_2 = c(X_2_first_half, X_2_second_half)
#' #half of the data comes from one distribution the other from the second
#' y = c(rep(1, 50), rep(2, 50))
#' X_test_data = cbind(X_1, X_2)
#' #will perfectly split the data
#' output <- MyKmeans(X_test_data, 2)

MyKmeans <- function(X, K, M = NULL, numIter = 100){
  
  n = nrow(X) # number of rows in X
  
  # Check whether M is NULL or not. If NULL, initialize based on K random points from X. If not NULL, check for compatibility with X dimensions.
  if(is.null(M) == TRUE){
    draws <- sample(1:nrow(X), K)
    M <- X[draws, ]
  }
  
  
  # If M is the right dimensions, this creates the clusters from M
  else if((is.null(M) == FALSE) && sum(dim(M) == c(K, dim(X)[2])) != 2){
    stop(paste('dim(M) != K \u00D7 ncol(X)'))
  }
  
  
  # Call C++ MyKmeans_c function to implement the algorithm
  Y = MyKmeans_c(X, K, M, numIter)
  
  # Return the class assignments
  return(Y)
}
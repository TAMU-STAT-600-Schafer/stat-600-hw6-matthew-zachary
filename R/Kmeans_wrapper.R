library(Rcpp)
library(RcppArmadillo)
Rcpp::sourceCpp("src/kmeans.cpp")
#' Title
#'
#' @param X 
#' @param K 
#' @param M 
#' @param numIter 
#'
#' @return Explain return
#' @export
#'
#' @examples
#' # Give example
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
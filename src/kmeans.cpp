// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::uvec MyKmeans_c(const arma::mat& X, int K,
                            const arma::mat& M, int numIter = 100){
    // All input is assumed to be correct
    
    // Initialize some parameters
    int n = X.n_rows;
    int p = X.n_cols;
    arma::uvec Y(n); // to store cluster assignments
    
    // Initialize any additional parameters if needed
    
    // For loop with kmeans algorithm
    for(int i = 0; i < numIter, ++i){
  // This calculates which cluster is closest to the data point
      Y <- apply(euc_function(X, X_squared, mu_1), MARGIN = 1, FUN = which.min);
      
  // This updates the cluster means
      for(i in 1:K){
        mu_temp[i, ] <- colMeans(X[Y == i, , drop = FALSE])
      }
      
  // This breaks the loop if the cluster means did not change from the previous iteration
      if(identical(mu_1,mu_temp)){
        break
      }
      
  // This checks to see if any of the clusters has disappeared and throws an error if one has
      if(length(unique(Y)) < K){
        stop(paste("One of the clusters has disappeared"))
      }
      
  // This resets the cluster means
      mu_1 <- mu_temp
    }
    
  // This removes the naming conventions of Y
    Y <- unname(Y)
    
    
    // Returns the vector of cluster assignments
    return(Y);
}


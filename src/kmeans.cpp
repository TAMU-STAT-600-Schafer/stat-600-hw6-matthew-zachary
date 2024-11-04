// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
arma::mat square_mat(const arma::mat& X, int& K){
  arma::vec squared;
  arma::mat X_squared(X.n_rows, X.n_cols);
  arma::mat returner(X.n_rows, K);
  
  for(int i = 0; i < X.n_cols; ++i){
    X_squared.col(i) = arma::square(X.col(i));
  }
  
  squared = sum(X_squared,1);
  
  for(int i = 0; i < K; ++i){
    returner.col(i) = squared;
  }
  
  return returner;
}

// [[Rcpp::export]]
arma::mat euc_function_c(const arma::mat& X, const arma::mat& X_squared, const arma::mat& mu){
  arma::mat cluster;
  arma::mat temp(mu.n_rows, 1);
  arma::mat temp_squared(X.n_rows, mu.n_rows);
  arma::mat returner(X.n_rows, mu.n_rows);
  
  temp = sum(arma::square(mu),1);
  
  for(int i = 0; i < mu.n_rows; ++i){
    for(int j = 0; j < X.n_rows; ++j){
      temp_squared.col(i).row(j) = temp(i);
    }
  }
  
  returner = X_squared + temp_squared - (2 * (X * mu.t()));
  
  return returner;
}

// [[Rcpp::export]]
arma::uvec MyKmeans_c(const arma::mat& X, int K,
                      const arma::mat& M, int numIter = 100){
  // All input is assumed to be correct
  
  // Initialize some parameters
  int n = X.n_rows;
  // int p = X.n_cols;
  arma::uvec Y(n); // to store cluster assignments
  
  // Initialize any additional parameters if needed
  arma::uvec indices;
  arma::mat mu_temp(K, n);
  arma::mat mu_1(K, n);
  arma::mat distances(n, K);
  arma::uvec unique_vals;
  
  mu_temp = M;
  mu_1 = M;
  
  // For loop with kmeans algorithm
  arma::mat X_squared = square_mat(X, K);
  
  for(int i = 0; i < numIter; ++i){
    distances = euc_function_c(X, X_squared, mu_1);
    
    for (int i = 0; i < distances.n_rows; ++i) {
      Y(i) = distances.row(i).index_min(); 
    }
    
    unique_vals = arma::unique(Y);
    
    if(unique_vals.size() < K){
      Rcpp::stop("One of the clusters disappeared");
    }
    
    for(int i = 0; i < K; ++i){
      indices = find(Y == (i));
      mu_temp.row(i) = mean(X.rows(indices), 0);
    }
    
    if (arma::approx_equal(mu_1, mu_temp, "absdiff", 1e-10)) {
      break; 
    }
    
    mu_1 = mu_temp;
  }
  
  // Returns the vector of cluster assignments
  return Y;
}


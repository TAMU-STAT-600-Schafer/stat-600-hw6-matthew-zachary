// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
bool identical(const arma::vec& a, const arma::vec& b){
  // This checks to see if the vectors are the same length
  if(a.size() != b.size()){
    return false;
  }
  
  // This checks to see if the vectors are the same values 
  for(int i = 0; i < a.size(); i++){
    if(a[i] != b[i]){
      return false;
    }
  }
  
  // If they are the exact same, it will return true
  return true;
}

// [[Rcpp::export]]
arma::mat testing(const arma::mat& X){
  arma::mat X_squared = sum(arma::pow(X,2), 1);
  return X_squared;
}

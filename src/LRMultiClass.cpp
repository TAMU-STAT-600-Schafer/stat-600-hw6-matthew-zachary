// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

//Implementing a Helper function that computes class probabilities 
//X - nxp data matrix
//beta - pxk beta values 
arma::mat class_probabilities(const arma::mat& X, const arma::mat&beta){
  int n = X.n_rows;
  int k = beta.n_cols;
  
  arma::mat exp_scores = X * beta;
  arma::mat probabilities(n, k);
  
  for(int i=0; i<n; i++){
    //check this, it's saying that sum isn't in namespace? but seems like its in the docs
    probabilities.row(i) = exp_scores.row(i)/arma::sum(exp_scores.row(i));
    
  }

  return probabilities;
  
}
//Implementing a Helper function that computes the objective value 
double objective_fx(const arma::mat& X, const arma::colvec& Y, const arma::mat& beta, double lambda, const arma::mat& class_probabilities) {
  int n = X.n_rows;
  int k = beta.n_rows;
  double first_term = 0.0;
  
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < k; j++) {
      if (Y(i) == j) {
        first_term += arma::log(class_probabilities(i, j));
      }
    }
  }
  
  double regularization_term = (lambda / 2) * arma::sum(arma::pow(beta, 2));
  double function_value = -first_term + regularization_term;
  
  return function_value;
}

// For simplicity, no test data, only training data, and no error calculation.
// X - n x p data matrix
// y - n length vector of classes, from 0 to K-1
// numIter - number of iterations, default 50
// eta - damping parameter, default 0.1
// lambda - ridge parameter, default 1
// beta_init - p x K matrix of starting beta values (always supplied in right format)
// [[Rcpp::export]]
Rcpp::List LRMultiClass_c(const arma::mat& X, const arma::uvec& y, const arma::mat& beta_init,
                               int numIter = 50, double eta = 0.1, double lambda = 1){
    // All input is assumed to be correct
    
    // Initialize some parameters
    int K = max(y) + 1; // number of classes
    int p = X.n_cols;
    int n = X.n_rows;
    arma::mat beta = beta_init; // to store betas and be able to change them if needed
    arma::vec objective(numIter + 1); // to store objective values
    
    // Initialize anything else that you may need
    
    // Newton's method cycle - implement the update EXACTLY numIter iterations
    
    
    // Create named list with betas and objective values
    return Rcpp::List::create(Rcpp::Named("beta") = beta,
                              Rcpp::Named("objective") = objective);
}

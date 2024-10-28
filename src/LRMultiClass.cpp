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
// [[Rcpp::export]]
arma::mat class_probabilities(const arma::mat& X, const arma::mat&beta){
  int n = X.n_rows;
  int k = beta.n_cols;
  
  arma::mat exp_scores = X * beta;
  arma::mat probabilities(n, k);
  
  for(int i=0; i<n; i++){
    //check this, it's saying that sum isn't in namespace? but seems like its in the docs
    probabilities.row(i) = exp_scores.row(i) / arma::accu(exp_scores.row(i));
    
  }

  return probabilities;
  
}
//Implementing a Helper function that computes the objective value 
// [[Rcpp::export]]
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

//Implementing a helper function for W_k
// [[Rcpp::export]]
arma::mat compute_W_k(const arma::mat& P_k) {
  return P_k % (1.0 - P_k);
}

//Implementing a helper function to update our Beta_k
//[[Rcpp:export]]
arma::colvec update_B_k(const arma::mat& X, const arma::mat& P_k, const arma::colvec& Y, int k, const arma::colvec& beta_k, double lambda, double eta) {
  int n = X.n_rows;
  int p = X.n_cols;
  
  arma::mat W_k = compute_W_k(P_k);
  arma::mat X_W_k = X % W_k;
  arma::mat X_T_W_k = arma::trans(X) * X_W_k;
  
  arma::colvec second_term = arma::trans(X) * (P_k - arma::conv_to<arma::colvec>::from(Y == (k - 1)));
  
  arma::colvec updated_beta_k = beta_k - eta * arma::solve(X_T_W_k + lambda * arma::eye(p, p), second_term + lambda * beta_k);
  return updated_beta_k;
}

arma::mat update_fx(const arma::mat& X, const arma::colvec& Y, const arma::mat& beta, double lambda, double eta, const arma::mat& probabilities) {
  int K = beta.n_cols;
  
  for (int i = 0; i < K; i++) {
    //fix why is this subview error happening? 
    beta.col(i) = update_B_k(X, probabilities, Y, i + 1, beta.col(i), lambda, eta);
  }
  
  return beta;
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

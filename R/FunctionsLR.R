# Function that implements multi-class logistic regression.
#############################################################
# Description of supplied parameters:
# X - n x p training data, 1st column should be 1s to account for intercept
# y - a vector of size n of class labels, from 0 to K-1
# Xt - ntest x p testing data, 1st column should be 1s to account for intercept
# yt - a vector of size ntest of test class labels, from 0 to K-1
# numIter - number of FIXED iterations of the algorithm, default value is 50
# eta - learning rate, default value is 0.1
# lambda - ridge parameter, default value is 1
# beta_init - (optional) initial starting values of beta for the algorithm, should be p x K matrix 

## Return output
##########################################################################
# beta - p x K matrix of estimated beta values after numIter iterations
# error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
# error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
# objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
LRMultiClassold <- function(X, y, Xt, yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  ## Check the supplied parameters as described. You can assume that X, Xt are matrices; y, yt are vectors; and numIter, eta, lambda are scalars. You can assume that beta_init is either NULL (default) or a matrix.
  ###################################
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if(!all(X[ , 1] == 1) || !all(Xt[ , 1] == 1)){
    stop(print("First column of X and/or Xt are not all 1s"))
  }
  # Check for compatibility of dimensions between X and Y
  if (dim(X)[1] != length(y)){
    stop(print("the dimensions of X and Y are not compatible"))
  }
  # Check for compatibility of dimensions between Xt and Yt
  if (dim(Xt)[1] != length(yt)){
    stop(print("the dimensions of Xt and Yt are not compatible"))
  }
  # Check for compatibility of dimensions between X and Xt
  if (dim(Xt)[2] != dim(X)[2]){
    stop(print("the dimensions of Xt and X are not compatible"))  
  }
  # Check eta is positive
  if(eta <= 0){
    stop(print("eta must be positive"))  
  }
  # Check lambda is non-negative
  if (lambda < 0){
    stop(print("lambda must be non-negative"))
  }
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes.
  if (is.null(beta_init)) {
    # Initialize beta with zeroes: p x K (number of features x number of classes)
    beta_init <- matrix(0, ncol(X), length(unique(y)))
  } else {
    # Check if the dimensions of beta_init are compatible: it should be p x K
    if (dim(beta_init)[1] != ncol(X) || dim(beta_init)[2] != length(unique(y))) {
      stop(paste("beta_init should be p x K but it is instead", dim(beta_init)[1], "x", dim(beta_init)[2]))
    }
  }
  
  ##Vectors to store training error, test error, and objective values
  error_train <- numeric(numIter + 1)
  error_test <- numeric(numIter + 1)
  objective <- numeric(numIter + 1)
  ## Calculate corresponding pk, objective value f(beta_init), training error and testing error given the starting point beta_init
  ##########################################################################
  initial_probabilities <- class_probabilities(X, beta_init)
  objective[1] <- objective_fx(X, y, beta_init, lambda, initial_probabilities)
  test_probs <- class_probabilities(Xt, beta_init)
  error_train[1] <- mean(classify(initial_probabilities) != y) * 100
  error_test[1] <- mean(classify(test_probs) != yt) * 100
  ## Newton's method cycle - implement the update EXACTLY numIter iterations
  ##########################################################################
  for (i in 2:(numIter+1)){
  # Within one iteration: perform the update, calculate updated objective function and training/testing errors in %
    #just need update_fx to do what we want 
    beta_init <- update_fx(X, y, beta_init, lambda, eta, initial_probabilities)
    initial_probabilities <- class_probabilities(X, beta_init)
    objective[i] <- objective_fx(X, y, beta_init, lambda, initial_probabilities)
    test_probs <- class_probabilities(Xt, beta_init)
    error_train[i] <- mean(classify(initial_probabilities) != y) * 100
    error_test[i] <- mean(classify(test_probs) != yt) * 100
  }
  beta <- beta_init
  ## Return output
  ##########################################################################
  # beta - p x K matrix of estimated beta values after numIter iterations
  # error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
  # error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
  # objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
  return(list(beta = beta, error_train = error_train, error_test = error_test, objective =  objective))
}

#Function that takes in a nxp matrix X with the data points and a pxk matrix Beta
class_probabilities_r <- function(X, beta){
  exp_scores <- exp(X %*% beta)
  probabilities <- exp_scores/rowSums(exp_scores)
  return (probabilities)
}
#function that takes in X, Y, Beta, and lambda as constructed above and addtionally
#the class probabilities and returns the objective function 
objective_fx_r <- function(X, Y, beta, lambda, class_probabilities){
  n <- nrow(X)
  k <- nrow(beta)
  first_term <- 0
  for (i in 1:n){
    for(class in 1:k){
      if(Y[i] == (class-1)){  
        first_term = first_term + log(class_probabilities[i, class])   
      }
      
    }
  }
  regularization_term = (lambda/2) * sum(beta^2)
  function_value = -first_term + regularization_term
  return (function_value)
}

#classifies each point given a the probabilities
classify_r <- function(probs){
  #potentially use max.col if that works cuz its faster 
  return(apply(probs, 1, which.max) - 1) 
}

#takes in probabilites for a class K and computes the diag 
compute_W_k_r <- function(P_k){
  W_k <- P_k * (1-P_k)    
  return(W_k)
}
update_B_k_r <- function(X, P_k, Y, k, beta_k, lambda, eta){
  n <- nrow(X)
  p <- ncol(X)
  W_k <- compute_W_k(P_k)
  
  X_W_k <- X * W_k
  #can I speed up this line by using crossprod? 
  X_T_W_k <- t(X) %*% X_W_k
  
  #second term calculation 
  second_term <- t(X) %*% (P_k - (Y==(k-1)))
  
  updated_beta_k <- beta_k - (eta * solve(X_T_W_k + (lambda * diag(p))) %*% (second_term + (lambda * beta_k)))
  return (updated_beta_k)  
}
update_fx_r <- function(X, Y, beta, lambda, eta, probabilities){
  K <- ncol(beta)
  for (i in 1:K){
    beta[ , i] <- update_B_k(X, probabilities[ , i], Y, i, beta[, i], lambda, eta)
  }
  return(beta)
}
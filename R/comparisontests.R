source("R/LR_wrapper.R")
source("R/FunctionsLR.R")
library(Rcpp)
library(RcppArmadillo)

#now testing the whole function
n_train <- 100   
n_test <- 100     
p <- 3           
K <- 4           

# Training data
X_train <- matrix(rnorm(n_train * p), nrow = n_train, ncol = p)
y_train <- sample(1:K, n_train, replace = TRUE)
X_train <- cbind(1, X_train)
# Testing data
X_test <- matrix(rnorm(n_test * p), nrow = n_test, ncol = p)
y_test <- sample(1:K, n_test, replace = TRUE)
X_test <- cbind(1, X_test)

old = LRMultiClassold(X_train, y_train, X_test, y_test)
new = LRMultiClass(X_train, y_train)


#new test for Multiclass C
X_test <- matrix(c(rep(1, 10), rnorm(30)), nrow = 10, ncol = 4)
y_test <- as.integer(sample(0:1, 10, replace = TRUE))
beta_init_test <- matrix(0, nrow = 4, ncol = 2)

X_test <- as.matrix(X_test)
y_test <- as.integer(y_test)
beta_init_test <- as.matrix(beta_init_test)
result <- LRMultiClass_c(X_test, y_test, beta_init_test, numIter = 5, eta = 0.1, lambda = 1)

#test for class_probs
X_test <- matrix(c(1, 2, 1, 3, 1, 4), ncol = 2, byrow = TRUE) # 3 x 2 matrix
beta_test <- matrix(c(0.1, 0.2, 0.3, 0.4), ncol = 2) # 2 x 2 matrix
probabilities_r <- class_probabilities_r(X_test, beta_test)
probabilities_cpp <- class_probabilities(X_test, beta_test)
print(all.equal(probabilities_r, probabilities_cpp))

#test for obj function 
# Define test data
X_test <- matrix(c(1, 2, 1, 3, 1, 4), ncol = 2, byrow = TRUE) # 3 x 2 matrix
Y_test <- c(0, 1, 1)
beta_test <- matrix(c(0.1, 0.2, 0.3, 0.4), ncol = 2) # 2 x 2 matrix
lambda_test <- 0.5
class_probs <- class_probabilities(X_test, beta_test) 
objective_r <- objective_fx_r(X_test, Y_test, beta_test, lambda_test, class_probs)
objective_cpp <- objective_fx(X_test, Y_test, beta_test, lambda_test, class_probs)
cat("Test `objective_fx`\n")
print(all.equal(objective_r, objective_cpp))

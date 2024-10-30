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
print(all.equal(objective_r, objective_cpp))


#test for w_k 
# Define test data
P_k_test <- matrix(c(0.2, 0.8, 0.3, 0.7, 0.4, 0.6), ncol = 2) # 3 x 2 matrix
W_k_r <- compute_W_k_r(P_k_test)
W_k_cpp <- compute_W_k(P_k_test)
print(all.equal(W_k_r, W_k_cpp))

#test for b_k
# Define test data
beta_k_test <- c(0.1, 0.2) # 2 x 1 vector
lambda_test <- 0.5
eta_test <- 0.01
updated_beta_k_r <- update_B_k_r(X_test, P_k_test[, 1], Y_test, 1, beta_k_test, lambda_test, eta_test)
updated_beta_k_cpp <- update_B_k(X_test, P_k_test, Y_test, 1, beta_k_test, lambda_test, eta_test) 
print(all.equal(updated_beta_k_r, updated_beta_k_cpp))

#testing for bigger b_k 
X_train_2 <- matrix(rnorm(50), nrow = 10, ncol = 5) # 10 observations, 5 features
Y_train_2 <- sample(0:2, 10, replace = TRUE)        # 10 class labels (3 classes)

# Initialize parameters
beta_2 <- matrix(rnorm(15), ncol = 3)    # p x K matrix (5 x 3)           
probabilities_2 <- class_probabilities(X_train_2, beta_2)
updated_beta_k_r <- update_B_k_r(X_train_2, probabilities_2[ , 1], Y_train_2, 1, beta_2[,1], lambda_test, eta_test)
updated_beta_k_cpp <- update_B_k(X_train_2, as.vector(probabilities_2[, 1]), Y_train_2, 0, beta_2[,1], lambda_test, eta_test) 
print(all.equal(updated_beta_k_r, updated_beta_k_cpp))

#test for updating our update_fx
probabilities <- class_probabilities(X_test, beta_test)

updated_beta_r <- update_fx_r(X_test, Y_test, beta_test, lambda_test, eta_test, probabilities)
updated_beta_cpp <- update_fx(X_test, Y_test, beta_test, lambda_test, eta_test, probabilities) 

print(all.equal(updated_beta_r, updated_beta_cpp))

#test for our actual function 
X <- matrix(c(1, 1, 1, 1, 1, 2, 3, 4, 5, 6, 
              1, 2, 1, 3, 1, 4, 2, 5, 3, 6), ncol = 4)
y <- c(0, 1, 2, 1, 0)
beta_init <- matrix(0, ncol(X), length(unique(y)))

numIter <- 10
eta <- 0.1
lambda <- 0.01

result_cpp <- LRMultiClass_c(X, y, beta_init, numIter, eta, lambda)
result_r <- LRMultiClassold(X, y, X, y, numIter, eta, lambda, beta_init)

source("R/LR_wrapper.R")
source("R/FunctionsLR.R")

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



#' Title
#'
#' @param X 
#' @param y 
#' @param numIter 
#' @param eta 
#' @param lambda 
#' @param beta_init 
#'
#' @return
#' @export
#'
#' @examples
#' # Give example
LRMultiClass <- function(X, y, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  
  # Compatibility checks from HW3 and initialization of beta_init
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if(!all(X[ , 1] == 1)){
    stop(print("First column of X are not all 1s"))
  }
  # Check for compatibility of dimensions between X and Y
  if (dim(X)[1] != length(y)){
    stop(print("the dimensions of X and Y are not compatible"))
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
  
  # Call C++ LRMultiClass_c function to implement the algorithm
  out = LRMultiClass_c(X, y, numIter, eta, lambda, beta_init)
  
  # Return the class assignments
  return(out)
}
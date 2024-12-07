% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Kmeans_wrapper.R
\name{MyKmeans}
\alias{MyKmeans}
\title{Title}
\usage{
MyKmeans(X, K, M = NULL, numIter = 100)
}
\arguments{
\item{X}{n by p matrix containing n data points to cluster}

\item{K}{integer specifying number of clusters}

\item{M}{optional, K by p matrix of cluster centers}

\item{numIter}{number of maximal iterations for the algorithm, the default value is 100}
}
\value{
Return the vector of assignments Y
}
\description{
Title
}
\examples{
# Give example
set.seed(123)
#constructing toy data 
X_first_half = rnorm(50, 0, 1)
X_second_half = rnorm(50, 100, 2)
X_2_first_half = rnorm(50, 5, 2)
X_2_second_half = rnorm(50, 95, 2)
X_1 = c(X_first_half, X_second_half)
X_2 = c(X_2_first_half, X_2_second_half)
#half of the data comes from one distribution the other from the second
y = c(rep(1, 50), rep(2, 50))
X_test_data = cbind(X_1, X_2)
#will perfectly split the data
output <- MyKmeans(X_test_data, 2)
}

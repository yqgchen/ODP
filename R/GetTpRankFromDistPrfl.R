#' @title Compute transport ranks from distance profiles
#' @description Compute transport ranks based on the distance profiles represented by quantile functions. 
#' For \eqn{x = X_i} within the training data \eqn{\{X_i\}_{i=1}^{n}}, 
#' the transport rank of \eqn{x} is estimated by comparing the distance profile of \eqn{x} with 
#' the distance profiles of \eqn{\{X_j\}_{j\neq i}}.
#' For a new \eqn{x} out of the training data, 
#' the transport rank of \eqn{x} is estimated by comparing the distance profile of \eqn{x} with 
#' the distance profiles of \eqn{\{X_j\}_{j=1}^{n}}.
#' @param qDistPrfl A list of two fields, \code{x} and \code{ymat}, holding the quantile functions 
#' corresponding to the distance profiles for each observation in training data and possibly new data. 
#' \describe{
#' \item{x}{A vector holding the (common) support grid of \eqn{(n+m)} quantile functions, 
#' in a strictly increasing order between 0 and 1.}
#' \item{ymat}{A matrix with \eqn{(n+m)} rows, each row holding the values of the 
#' quantile function for one data point.} 
#' }
#' Here, \eqn{n} is the size of the sample with respect to which distance profiles 
#' have been obtained and \eqn{m} is the size of the new sample in addition 
#' distance profiles have been obtained with respect to the sample of size \eqn{n}. 
#' @param n The size of the sample with respect to which distance profiles have been obtained, 
#' i.e. \eqn{n} in the explanation of \code{qDistPrfl}. Default: \code{nrow(qDistPrfl$ymat)}.
#' @param fun A function giving the monotonic transformation applied for certain purpose, 
#' e.g., making ranks lying between 0 and 1. Default: \code{\link{expit}}.
#' @details The argument \code{qDistPrfl} can be obtained by using \code{\link{GetDistPrfl}} 
#' with \code{type = 'qf'} or \code{type = 'all'}, and the field named \code{'qf'} 
#' in the output list yields input of \code{qDistPrfl}. 
#' @return A vector holding the transport ranks.
#' @examples
#' d <- 2
#' n <- 10
#' m <- 5
#' data <- matrix( rnorm( n * d ), ncol = d )
#' newdata <- matrix( rnorm( m * d ), ncol = d )
#' distmat <- as.matrix( stats::dist( rbind(data,newdata) ) )[, 1:n, drop=FALSE]
#' profile <- GetDistPrfl( distmat = distmat )
#' res <- GetTpRankFromDistPrfl( qDistPrfl = profile$qf, n = n )
#' @export
#' @importFrom pracma trapz
#' 
GetTpRankFromDistPrfl <- function ( qDistPrfl, n, fun = expit ) {
  # with expit; only for sample points X_i's
  n_all <- nrow(qDistPrfl$ymat)
  if ( missing(n) ) {
    n <- n_all
  }
  q_train <- qDistPrfl$ymat[seq_len(n), , drop = FALSE]
  rank <- colMeans(
    sapply( seq_len(n), function(i) {
      qDiff <- q_train[-i, , drop = FALSE] - matrix(qDistPrfl$ymat[i,], nrow = n-1, ncol = ncol(qDistPrfl$ymat), byrow = TRUE)
      apply( qDiff, 1, pracma::trapz, x = qDistPrfl$x )
    })
  )
  if ( n_all > n ) {
    rank <- c(
      rank, 
      colMeans(
        sapply( (n+1):n_all, function(i) {
          qDiff <- q_train - matrix(qDistPrfl$ymat[i,], nrow = n, ncol = ncol(qDistPrfl$ymat), byrow = TRUE)
          apply( qDiff, 1, pracma::trapz, x = qDistPrfl$x )
        })
      )
    )
  }
  return ( fun(rank) )
  
}

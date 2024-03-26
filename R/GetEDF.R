#' @title Compute empirical distribution functions
#' @description For a sample of real-valued random variables \eqn{X_1,\dots,X_n}, compute the empirical distribution function 
#' \eqn{\hat{F}(x) = \frac{1}{n}\sum_{i=1}^{n} \chi\{X_i\le x\}}, 
#' where \eqn{\chi\{\cdot\}} denotes the indicator function. 
#' @param x A numeric vector holding the data.
#' @param sup A numeric vector holding the support grid of the empirical distribution function.
#' @param returnSup Logical; indicating whether \code{sup} is to be returned. Default: \code{FALSE}.
#' @return A vector holding the empirical distribution function evaluated on \code{sup} 
#' if \code{returnSup} is \code{FALSE}; 
#' A list containing two fields \code{x} and \code{y} holding the support grid and 
#' the values of the empirical distribution function, respectively, 
#' if \code{returnSup} is \code{TRUE}. 
#' @examples 
#' x <- runif(50)
#' edf <- GetEDF( x = x, sup = seq(0,1,0.01), returnSup = TRUE )
#' @export
#' 
GetEDF <- function ( x, sup, returnSup = FALSE ) {
  if( is.unsorted(x) ) {
    x.sorted <- sort(x)
  } else {
    x.sorted <- x
  }
  edf <- findInterval(
    x = sup, vec = x.sorted,
    rightmost.closed = FALSE, all.inside = FALSE,
    left.open = FALSE
  ) / length(x)
  if ( returnSup ) {
    return( list( x = sup, y = edf ) )
  } else {
    return( edf )
  }
}

# too slow
## empirical cdf
# GetEDF <- function (x, sup) {
#   len <- length(x)
#   list(
#     x = sup,
#     y = colMeans(
#       matrix( x, ncol = length(sup), nrow = len ) -
#         matrix( sup, ncol = length(sup), nrow = len, byrow = TRUE ) <= 0
#     )
#   )
# } 
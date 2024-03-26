#' @title Compute empirical quantile functions
#' @description For a sample of real-valued random variables \eqn{X_1,\dots,X_n}, compute the empirical quantile function 
#' \eqn{\hat{Q}(p) = \inf\{x: \hat{F}(x) \ge p\}}, where \eqn{\hat{F}(\cdot)} denotes 
#' the corresponding empirical distribution function.
#' @param x A numeric vector holding the data.
#' @param qSup A numeric vector holding the support grid of the empirical quantile function.
#' @param returnSup Logical; indicating whether \code{sup} is to be returned. Default: \code{FALSE}.
#' @return A vector holding the empirical quantile function evaluated on \code{qSup} 
#' if \code{returnSup} is \code{FALSE}; 
#' A list containing two fields \code{x} and \code{y} holding the support grid and 
#' the values of the empirical quantile function, respectively, 
#' if \code{returnSup} is \code{TRUE}. 
#' @examples 
#' x <- runif(50)
#' edf <- GetEQF( x = x, qSup = seq(0,1,0.01), returnSup = TRUE )
#' @export
#' 
GetEQF <- function ( x, qSup, returnSup = FALSE ) {
  len <- length(x)
  x.sorted <- sort(x)
  if ( any( qSup > 1 ) ) {
    qSup[ qSup > 1 ] <- 1
  }
  eqf <- numeric( length(qSup) )
  eqf[ qSup > 0 ] <- x.sorted[ ceiling( qSup[ qSup > 0 ] * len ) ]
  # if ( any( qSup == 0 ) ) {
  #   extrapolated <- x.sorted[1] - ( x.sorted[len] - x.sorted[1] ) / ( length(qSup) - 1 ) / 2
  #   eqf[ which( qSup == 0 ) ] <- max( 0, eqf[ which( qSup == 0 ) ] )
  # }
  if ( any( qSup == 0 ) ) {
    eqf[ which( qSup == 0 ) ] <- x.sorted[1]
  }
  if ( returnSup ) {
    return ( list( x = qSup, y = eqf ) )
  } else {
    return (eqf)
  }
}
# slower: 
# GetEQF <- function (x, qSup) {
#   len <- length(x)
#   if ( is.unsorted(x) ) {
#     x.sorted <- sort(x)
#   } else {
#     x.sorted <- x
#   }
#   list(
#     x = qSup,
#     y = x.sorted[
#       findInterval(
#         x = qSup, vec = seq(0, 1, length.out = len + 1 ),
#         all.inside = TRUE,
#         left.open = TRUE
#       )
#     ]
#   )
# }
#' @title Get pairwise distances between observations (data objects)
#' @param data Either a matrix or data frame with \eqn{n} rows where each row holds one observation 
#' in the (training) data, or a list (but not a data frame) of length \eqn{n} where each element holds one observation.
#' @param distfun A function with (at least) two arguments \code{x} and \code{y} 
#' computing the distance between two observations \code{x} and \code{y}. 
#' Default: Euclidean distance \code{function (x,y) {sqrt(sum((x-y)^2))}}.
#' @param newdata Either a matrix or data frame with \eqn{m} rows and the same number of columns as \code{data} 
#' where each row holds one new observation in addition to the observations in \code{data}, 
#' or a list (but not a data frame) of length \eqn{m} where each element is of the same format as those in \code{data} 
#' and holds one new observation. 
#' Pairwise distances from observations in \code{newdata} to those in \code{data} are to be computed. 
#' @param ... Optional additional arguments of \code{distfun}.
#' @return An \eqn{(n+m)}-by-\eqn{n} matrix holding the pairwise distances between observations.
#' @examples 
#' ## 2-Wasserstein distance for distributions on the real line
#' mu <- 1:4
#' qSup <- seq(0,1,len=1001)[-c(1,1001)]
#' data <- lapply( mu, qnorm, p = qSup )
#' distmat <- GetPairDist( data = data, distfun = l2metric, sup = qSup )
#' @export
#' @importFrom utils combn
#'
GetPairDist <- function(data, distfun = NULL, newdata = NULL, ...) {
  if (is.null(distfun)) {
    distfun = function(x,y) {
      sqrt(sum((x-y)^2))
    }
  }
  
  ReformatData <- function (data, varname = 'data') {
    if ( is.data.frame(data) | is.matrix(data) ) {
      n <- nrow( data )
      data <- lapply( seq_len(n), function (i) { data[i,] } )
    } else if ( !is.list( data ) ) {
      stop ( paste0( "wrong format of ", varname ) )
    }
    return ( data )
  }
  
  data <- ReformatData( data )
  n <- length(data)
  
  if ( !is.null(newdata) ) {
    newdata <- ReformatData( newdata, 'newdata' )
    m <- length(newdata)
  }
  
  if ( n < 2 ) {
    stop ( "The sample size cannot be less than 2." )
  }
  
  pairs <- combn(seq_len(n), 2)
  distmat <- matrix(0, ncol = n, nrow = n)
  distmat[t(pairs)] <- distmat[t(pairs[2:1,,drop=FALSE])] <-
    apply(pairs, 2, function(idx) {
      distfun( data[[idx[1]]], data[[idx[2]]], ...)
    })
  
  if (!is.null(newdata)) {
    distmat <- rbind(
      distmat, 
      sapply( data, function(x){
        sapply(newdata, distfun, x = x, ... )
      }) #m-by-n
    )
  }
  
  distmat
}

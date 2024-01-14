#' @title Compute transport ranks from a distance matrix or data
#' @description Compute transport ranks based on a distance matrix or data. 
#' For \eqn{x = X_i} within the training data \eqn{\{X_i\}_{i=1}^{n}}, 
#' the transport rank of \eqn{x} is estimated by comparing the distance profile of \eqn{x} with 
#' the distance profiles of \eqn{\{X_j\}_{j\neq i}}. 
#' For a new \eqn{x} out of the training data, 
#' the transport rank of \eqn{x} is estimated by comparing the distance profile of \eqn{x} with 
#' the distance profiles of \eqn{\{X_j\}_{j=1}^{n}}.
#' @param distmat An \eqn{(n+m)}-by-\eqn{n} matrix holding the pairwise distances between observations 
#' in training data of size \eqn{n} and possibly new data of size \eqn{m} to the training data.
#' If there is no new data of interest, then \code{distmat} should be a symmetric matrix of dimension \eqn{n}. 
#' At least one of \code{data} and \code{distmat} should be input. 
#' If both are given, \code{distmat} overrides \code{data}. 
#' @param data Training data with respect to the law of which distance profiles are of interest. 
#' Either a matrix or data frame with \eqn{n} rows where each row holds one observation 
#' in the training data, or a list (but not a data frame) of length \eqn{n} where each element holds one observation.
#' @param distfun A function with two arguments computing the distance between two observations, 
#' which is used in \code{\link{GetPairDist}}. Default: Euclidean distance.
#' @param newdata Optional new data, at which distance profiles are to be computed 
#' with respect to the law of \code{data}. 
#' Either a matrix or data frame with \eqn{m} rows and the same number of columns as \code{data} 
#' where each row holds one new observation in addition to the observations in \code{data}. 
#' or a list (but not a data frame) of length \eqn{m} where each element is of the same format as those in \code{data} 
#' and holds one new observation. 
#' This can only be specified if \code{data} but not \code{distmat} is given.
#' @param output_profile Logical, whether distance profiles need to be returned. Default: \code{TRUE}.
#' @param type Character specifying the type of representations of distance profiles to be output:
#' \code{'qf'} (default) for quantile functions,
#' and \code{'all'} for densities, cumulative distribution functions, and quantile functions.
#' @param optns A list of options control parameters specified by \code{list(name=value)}. See 'Details'.
#' @param ... Optional arguments of \code{distfun}.
#' @details This function is an intergration of integrating \code{\link{GetDistPrfl}} and \code{\link{GetTpRankFromDistPrfl}}. 
#' Cumulative distribution function and quantile function representations of 
#' distance profiles are obtained as the right-continuous empirical distribution functions 
#' and their left-continuous inverse, respectively. 
#' If density representation of distance profiles is of interest 
#' (\code{type = 'den'} or \code{type = 'all'}), 
#' density estimation is performed using \code{\link[frechet]{CreateDensity}}.
#' Available control options are \code{userBwMu}, \code{nRegGrid}, \code{delta}, \code{kernel}, 
#' \code{infSupport}, \code{outputGrid}, \code{nqSup} and \code{qSup}. 
#' See \code{\link[frechet]{CreateDensity}} for the details about the options 
#' other than \code{nqSup} and \code{qSup}.
#' \describe{
#' \item{nqSup}{The number of equidistant points in the support grid of the quantile functions. 
#' Default: 101.}
#' \item{qSup}{The support grid of the quantile functions; it overrides \code{nqSup}. 
#' Default: \code{seq(0,1,length.out = optns$nqSup)}.}
#' }
#' @return A list of the following fields:
#' \item{rank}{A vector holding the transport ranks.}
#' \item{n}{The size of the training data.}
#' \item{profile}{A list of the following:
#' \describe{
#' \item{den}{Density representations of distance profiles, only returned if \code{type='all'}.}
#' \item{cdf}{Cumulative distribution function representations of distance profiles, only returned if \code{type='all'}.}
#' \item{qf}{Quantile function representations of distance profiles.}
#' }}
#' \item{distmat}{An \eqn{(n+m)}-by-\eqn{n} matrix holding the pairwise distances between observations 
#' in training data of size \eqn{n} and possibly new data of size \eqn{m} to the training data. 
#' This will be output only if \code{data} but not \code{distmat} is given in the input.}
#' @examples
#' d <- 2
#' n <- 10
#' m <- 5
#' data <- matrix( rnorm( n * d ), ncol = d )
#' newdata <- matrix( rnorm( m * d ), ncol = d )
#' distmat <- as.matrix( stats::dist( rbind(data,newdata) ) )[, 1:n, drop=FALSE]
#' ## with input distmat
#' res <- GetTpRank( distmat = distmat )
#' ## Or with input data and newdata
#' res2 <- GetTpRank( data = data, newdata = newdata )
#' @seealso \code{\link{GetDistPrfl}} and \code{\link{GetTpRankFromDistPrfl}}. 
#' @export
#' 
GetTpRank <- function ( distmat, data, distfun = NULL, newdata = NULL, output_profile = TRUE, type = 'qf', optns = list(), ... ) {
  
  if ( output_profile ) {
    if ( !type %in% c('qf','all') ) {
      stop ( "Wrong input of type." )
    }
  } else {
    type <- 'qf'
  }
  
  profile <- GetDistPrfl( distmat = distmat, data = data, distfun = distfun, newdata = newdata, type = type, optns = optns, ... )
  
  rank <- GetTpRankFromDistPrfl( qDistPrfl = profile$qf, n = profile$n, fun = expit )
  
  res <- list( rank = rank, n = profile$n )
  if ( output_profile ) {
    res$profile <- profile[ names(profile) %in% c('den','cdf','qf') ]
  }
  if ( missing(distmat) ) {
    res$distmat <- profile$distmat
  }
  
  return ( res )
}
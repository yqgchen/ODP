#' @title Compute distance profiles with respect to a sample of random objects
#' @description Compute the distribution of \eqn{d(x,X)} for given \eqn{x}. 
#' For \eqn{x = X_i} within the training data \eqn{\{X_i\}_{i=1}^{n}}, 
#' the distribution of \eqn{d(x,X)} is estimated based on \eqn{\{d(x,X_j)\}_{j\neq i}}.
#' For a new \eqn{x} out of the training data, 
#' the distribution of \eqn{d(x,X)} is estimated based on \eqn{\{d(x,X_j)\}_{j=1}^{n}}.
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
#' @param type Character specifying the type of results to be computed:
#' \code{'den'} for densities,
#' \code{'cdf'} for cumulative distribution functions,
#' \code{'qf'} (default) for quantile functions,
#' and \code{'all'} for all of the three aforementioned types.
#' @param optns A list of control parameters specified by \code{list(name=value)}. See 'Details'.
#' @param ... Optional arguments of \code{distfun}.
#' @details Cumulative distribution function and quantile function representations of 
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
#' Default: \code{seq(0,1,length.out=optns$nqSup)}.}
#' }
#' @return A list. All three fields \code{den} (for the densities), \code{cdf} (for the cdfs) 
#' and \code{qf} (for the quantile functions) will be included if \code{type == 'all'}, 
#' and the corresponding field alone out of the three otherwise.
#' \item{den}{A list of \eqn{n} or \eqn{(n+m)} fields if \code{optns$outputGrid} is specified, 
#' each of which is a list of three fields, \code{bw}, \code{x} and \code{y}; 
#' see 'Value' of \code{\link[frechet]{CreateDensity}} for further details.
#' If \code{optns$outputGrid} is unspecified, 
#' it is a list of three fields, \code{bwvec}, \code{x} and \code{ymat}, where 
#' \code{bwvec} is a vector of length \eqn{n} or \eqn{(n+m)} holding the bandwidths used for smoothing,
#' \code{x} is the (common) support grid of the \eqn{n} or \eqn{(n+m)} densities specified by \code{optns$outputGrid}, 
#' and \code{ymat} is a matrix with \eqn{n} or \eqn{(n+m)} rows, each row holding the values of the density for one subject.}
#' \item{cdf}{A list of \eqn{n} or \eqn{(n+m)} fields if \code{optns$outputGrid} is specified, 
#' each of which is a list of two fields, \code{x} and \code{y}, 
#' which are two vectors holding the support grid and the corresponding values of the cdf, respectively.
#' If \code{optns$outputGrid} is unspecified, 
#' it is a list of two fields, \code{x} and \code{ymat}, where 
#' \code{x} is the (common) support grid of the \eqn{n} or \eqn{(n+m)} cdfs specified by \code{optns$outputGrid}, 
#' and \code{ymat} is a matrix with \eqn{n} or \eqn{(n+m)} rows, each row holding the values of the cdf for one subject.}
#' \item{qf}{A list of two fields, \code{x} and \code{ymat}, where 
#' \code{x} is a vector holding the (common) support grid of the \eqn{n} or \eqn{(n+m)} quantile functions, 
#' and \code{ymat} is a matrix with \eqn{n} or \eqn{(n+m)} rows, each row holding the values of the quantile function for one subject.}
#' \item{n}{The sample size of the training data.}
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
#' res <- GetDistPrfl( distmat = distmat )
#' ## Or with input data and newdata
#' res2 <- GetDistPrfl( data = data, newdata = newdata )
#' @export
#' @importFrom frechet CreateDensity
#' 
GetDistPrfl <- function ( distmat, data, distfun = NULL, newdata = NULL, type = 'qf', optns = list(), ... ) {
  if ( ! type %in% c('den','cdf','qf','all') ){
    stop ("Unrecognized input of type.")
  }
  missing_data <- missing( data )
  missing_distmat <- missing( distmat )
  
  if ( missing_data & missing_distmat ) {
    stop ( "Missing value of data and distmat: at least one of them should be specified." )
  }
  m <- 0 # sample size of newdata
  if ( !missing_distmat ) {
    if ( !missing_data ) {
      warning ( "The input distmat overrides the input data when both are specified." )
    }
    if ( !is.null(newdata) ) {
      stop ( "The argument newdata should only be input when the argument data is given. The input newdata is not used." )
    }
    
    if( diff(dim(distmat)) > 0 ) {
      distmat <- t(distmat)
      warning ( "The input distmat has more columns than rows and hence is transposed." )
    }
    
    if ( diff(dim(distmat)) < 0 ) {
      n <- ncol(distmat)
      m <- nrow(distmat) - n
    } else {
      n <- ncol(distmat)
    }
    
    CheckDistmat(distmat[1:n,1:n,drop = FALSE])
    
    # if ( any( distmat[lower.tri(distmat)] < 0 ) ) {
    #   stop ( "The input distmat has negative entries.")
    # }
  } else {
    distmat <- GetPairDist( data = data, distfun = distfun, newdata = newdata, ... )
    n <- ncol(distmat)
    m <- nrow(distmat) - n
  }
  
  # check whether outputGrid covers the range of distmat
  if ( !is.null(optns$outputGrid) ) {
    rangeDistmat <- range( distmat[ lower.tri( distmat, diag = FALSE ) | upper.tri( distmat, diag = FALSE ) ] )
    if ( min(optns$outputGrid) > rangeDistmat[1] | max(optns$outputGrid) < rangeDistmat[2] ) {
      stop ("optns$outputGrid does not cover the range of the pairwise distances.")
    }
  }
  
  if ( type %in% c('den','all') ) {
    den <- lapply(seq_len(n), function(i) {
      CreateDensity( y = distmat[i,-i], optns = optns )
    })
    if ( m > 0 ) {
      den <- c(
        den,
        lapply(seq_len(m)+n, function(i) {
          CreateDensity( y = distmat[i,], optns = optns )
        })
      )
    }
    
    if ( !is.null(optns$outputGrid) ) {
      den <- list(
        bwvec = sapply(den, function(d) d$bw),
        x = optns$outputGrid,
        ymat = t(sapply(den, function(d) d$y))
      )
    }
    if ( type == 'den' ) {
      res <- list( den = den, n = n )
    }
  }
  
  if ( type %in% c('cdf','all') ) {
    
    if ( is.null(optns$outputGrid) ) {
      if ( is.null(optns$nRegGrid) ) {
        optns$nRegGrid <- 101
      }
      ## set support for EDF
      setSup <- function (x, len) {
        ran <- range(x)
        ran <- ran + diff(ran) * 0.05 * c(-1,1)
        seq(ran[1], ran[2], len = len)
      }
      cdf <- lapply( seq_len(n), function(i) {
        GetEDF( x = distmat[i,-i], sup = setSup( x = distmat[i,-i], len = optns$nRegGrid ), returnSup = TRUE )
      })
      if ( m > 0 ) {
        cdf <- c(
          cdf,
          lapply(seq_len(m)+n, function(i) {
            GetEDF( x = distmat[i,], sup = setSup( x = distmat[i,], len = optns$nRegGrid ), returnSup = TRUE )
          })
        )
      }
    } else {
      cdf <- lapply( seq_len(n), function(i) {
        GetEDF( x = distmat[i,-i], sup = optns$outputGrid, returnSup = TRUE )
      })
      if ( m > 0 ) {
        cdf <- c(
          cdf,
          lapply(seq_len(m)+n, function(i) {
            GetEDF( x = distmat[i,], sup = optns$outputGrid, returnSup = TRUE )
          })
        )
      }
    }
    
    if ( !is.null(optns$outputGrid) ) {
      cdf <- list(
        x = optns$outputGrid,
        ymat = t(sapply(cdf, function(d) d$y))
      )
    }
    
    if ( type == 'cdf' ) {
      res <- list( cdf = cdf, n = n )
    }
  }
  
  if ( type %in% c('qf', 'all') ) {
    if ( is.null(optns$nqSup) ) optns$nqSup <- 101
    if ( is.null(optns$qSup) ) optns$qSup <- seq(0,1,length.out = optns$nqSup)
    
    qf <- list(
      x = optns$qSup,
      ymat = t( sapply( seq_len(n), function(i) {
        GetEQF( x = distmat[i,-i], qSup = optns$qSup, returnSup = FALSE )
      }) )
    )
    if ( m > 0 ) {
      qf$ymat <- rbind(
        qf$ymat,
        t(sapply( seq_len(m)+n, function(i) {
          GetEQF( x = distmat[i,], qSup = optns$qSup, returnSup = FALSE )
        }))
      )
    }
    if ( type == 'qf' ) {
      res <- list( qf = qf, n = n )
    }
  }
  
  if ( type == 'all' ) {
    res <- list( den = den, cdf = cdf, qf = qf, n = n )
  }
  if ( missing_distmat ) {
    res$distmat <- distmat
  }
  return (res)
}

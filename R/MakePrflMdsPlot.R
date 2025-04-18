#' @title Make a profile MDS plot
#' @description Make a plot of 2-dimensional classical metric multidimensional scaling of the 
#' data based on the distance profiles with respect to \eqn{2}-Wasserstein metric.
#' @param qDistPrfl A list of two fields, \code{x} and \code{ymat}, holding the quantile functions 
#' corresponding to the distance profiles for each observation in the data. 
#' \describe{
#' \item{x}{A vector holding the (common) support grid of \eqn{n} quantile functions, 
#' in a strictly increasing order between 0 and 1.}
#' \item{ymat}{A matrix with \eqn{n} rows, each row holding the values of the 
#' quantile function for one data point.} 
#' }
#' @param color_by A vector of length \eqn{n} holding the variable according to which 
#' colors are assigned to each point. Default: transport ranks computed based on \code{qDistPrfl}.
#' @param nGroup Number of groups for optional grouping according to \code{color_by}. 
#' If given, the sample is divided into \code{nGroup} groups according to quantiles of \code{color_by}, 
#' which should be numeric in this case; the \eqn{i}-th group contains the 
#' points with the corresponding value in \code{color_by} falling in 
#' \eqn{( q_{1-i/\code{nGroup}}, q_{1-(i-1)/\code{nGroup}} ] }, 
#' except for \eqn{i=\code{nGroup}}, for which the corresponding interval is \eqn{[ q_{0}, q_{1/\code{nGroup}} ]}. 
#' Here, \eqn{q_{\alpha}} is the \eqn{\alpha}-quantile of \code{color_by}, given by \code{quantile(color_by,alpha)}. 
#' Default: 10 if the sample size \eqn{n} is larger than 10 and \eqn{n} otherwise.
#' @param ... Other arguments of \code{\link{MakeObjMdsPlot}}.
#' @return A ggplot object.
#' @examples 
#' n <- 100
#' p <- 2
#' set.seed(1)
#' data <- matrix( rnorm( n * p ), ncol = p )
#' res <- GetTpRank( data = data )
#' pl <- MakePrflMdsPlot( qDistPrfl = res$profile$qf, color_by = res$rank, nGroup = 10 )
#' @seealso \code{\link{GetTpRank}}, \code{\link{MakeObjMdsPlot}}
#' @importFrom pracma trapz
#' @export
#' 
MakePrflMdsPlot <- function ( qDistPrfl, color_by, nGroup, ... ) {
  
  distmat <- GetPairDist( data = qDistPrfl$ymat, distfun = l2metric, sup = qDistPrfl$x )
  
  n <- nrow( qDistPrfl$ymat )
  
  if ( missing(color_by) ) {
    color_by <- GetTpRankFromDistPrfl( qDistPrfl = qDistPrfl, n = n, fun = expit )
  }
  
  if ( missing(nGroup) ) {
    nGroup <- min( 10, n )
  }
  MakeObjMdsPlot( distmat = distmat, color_by = color_by, nGroup = nGroup, ... )
}

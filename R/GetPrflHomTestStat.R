#' @title Compute the test statistic of the distance profile based two-sample homogeneity test for random objects
#' @description Compute the test statistic of the two-sample test for random objects based on comparing the distance profiles.
#' @param distmat An \eqn{(n+m)}-by-\eqn{(n+m)} matrix holding the pairwise distances between observations 
#' in the two samples of sizes \eqn{n} and \eqn{m}, respectively. 
#' @param n Size of the first sample.
#' @param nRegGrid Number of equidistant grid points from the minimum to the maximum of all pairwise distances,
#' on which the integrals of difference between distance profiles will be taken. Default: 101.
#' @return The value of the test statistic.
#' @examples
#' n <- m <- 50
#' data <- rep( rnorm(n,0,1), 2 )
#' distmat <- abs( matrix( data, nr = n+m, nc = n+m ) - matrix( data, nr = n+m, nc = n+m, byrow = TRUE ) )
#' stat <- GetPrflHomTestStat( distmat = distmat, n = n )
#' @export
#' 
GetPrflHomTestStat <- function ( distmat, n, nRegGrid = 101 ) {
  
  m <- nrow(distmat) - n
  outputGrid <- range(distmat)
  outputGrid <- seq( outputGrid[1], outputGrid[2], length.out = nRegGrid )
  
  prflX <- GetDistPrfl(
    distmat = distmat[,seq_len(n)],
    type = 'cdf',
    optns = list( outputGrid = outputGrid )
  )$cdf$ymat
  # first n rows -> profiles of X w.r.t. X;
  # remaining m rows -> profiles of Y w.r.t. X
  
  prflY <- GetDistPrfl(
    distmat = distmat[ c( seq_len(m)+n, seq_len(n) ), seq_len(m)+n ],
    type = 'cdf',
    optns = list( outputGrid = outputGrid )
  )$cdf$ymat[ c( seq_len(n)+m, seq_len(m) ), ]
  # first n rows -> profiles of X w.r.t. Y;
  # remaining m rows -> profiles of Y w.r.t. Y
  
  tmp <- ( prflX - prflY ) ^ 2
  tmp2 <- cbind( tmp[,2:(nRegGrid-1)], (tmp[,1]+tmp[,nRegGrid])/2 )
  d2 <- rowSums( tmp2 ) * diff(outputGrid[1:2])
  
  ( mean( d2[seq_len(n)] ) + mean( d2[seq_len(m)+n]) ) / (1/m+1/n)
  
}

#' @title Perform two-sample homogeneity test for random objects
#' @description Perform two-sample test for random objects based on comparing the distance profiles.
#' @param distmat An \eqn{(n+m)}-by-\eqn{(n+m)} matrix holding the pairwise distances between observations 
#' in the two samples of sizes \eqn{n} and \eqn{m}, respectively. 
#' @param n Size of the first sample.
#' @param nRegGrid Number of equidistant grid points from the minimum to the maximum of all pairwise distances,
#' on which the integrals of difference between distance profiles will be taken. Default: 101.
#' @param nPerm Number of permutations. Default: 999.
#' @return The \eqn{p}-value based on permutations.
#' @examples
#' n <- m <- 50
#' data <- rep( rnorm(n,0,1), 2 )
#' distmat <- abs( matrix( data, nr = n+m, nc = n+m ) - matrix( data, nr = n+m, nc = n+m, byrow = TRUE ) )
#' pval <- prflHomTest( distmat = distmat, n = n )
#' @export
#' 
prflHomTest <- function ( distmat, n, nRegGrid = 101, nPerm = 999 ) {
  testStat <- GetPrflHomTestStat( distmat = distmat, n = n, nRegGrid = nRegGrid )
  ntot <- nrow(distmat) # n+m
  testStat.boot <- lapply( seq_len(nPerm), function (i) {
    ind <- sample( seq_len(ntot), replace = FALSE )
    GetPrflHomTestStat( distmat = distmat[ind,ind], n = n, nRegGrid = nRegGrid )
  })
  
  # @importFrom foreach %dopar% foreach
  # @importFrom doParallel registerDoParallel
  # @importFrom parallel detectCores makeCluster stopCluster
  # if ( is.null(nCores) ) {
  #   nCores <- detectCores()
  # }
  # if ( nCores == 1 ) {
  #   testStat.boot <- lapply( seq_len(nPerm), function (i) {
  #     ind <- sample( seq_len(ntot), replace = FALSE )
  #     GetPrflHomTestStat( distmat = distmat[ind,ind], n = n, nRegGrid = nRegGrid )
  #   })
  # } else {
  #   cl = makeCluster(nCores); on.exit(stopCluster(cl)); registerDoParallel(cl)
  #   testStat.boot <- foreach( i = seq_len(nPerm), .packages = "ODP" ) %dopar% {
  #     ind <- sample( seq_len(ntot), replace = FALSE )
  #     GetPrflHomTestStat( distmat = distmat[ind,ind], n = n, nRegGrid = nRegGrid )
  #   }
  # }
  
  return ( sum( testStat.boot >= testStat ) / ( nPerm + 1 ) )
}

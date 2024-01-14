#' @title Check distance matrices
#' @description Check whether the input distance matrix or its sub square matrix is symmetric 
#' and has zero diagonals.
#' @param distmat An \eqn{n}-by-\eqn{n} matrix holding the pairwise distances between observations. 
#' @export
#' 
CheckDistmat <- function ( distmat ) {
  dimvec <- dim(distmat)
  n <- min(dimvec)
  if ( diff(dimvec) != 0 ) {
    A <- distmat[1:n,1:n]
  } else {
    A <- distmat
  }
  if ( !isSymmetric(A) ) {
    stop ( "The input distmat or its upper square sub-matrix is not symmetric." )
  }
  
  diag_A <- diag(A)
  names(diag_A) <- NULL
  if ( !isTRUE( all.equal( rep( 0, n ), diag_A ) ) ) {
    stop ( "The input distmat has non-zero diagonals." )
  }
}
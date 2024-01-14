#' @title \eqn{L^2} metric between square integrable functions.
#' @param x,y Vectors holding the values of two functions evaluated on \code{sup}.
#' @param sup Support grid.
#' @return The \eqn{L^2} metric between the two functions.
#' @export
#' 
l2metric <- function ( x, y, sup ) {
  sqrt( trapz( sup, (x-y)^2 ) )
}

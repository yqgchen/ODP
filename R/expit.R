#' Expit function
#' @description Compute the value of \eqn{\exp(x)/(1 + \exp(x))} for given \eqn{x}.
#' @param x A vector holding the real valued input.
#' @return A vector holding the result.
#' @export
#' 
expit <- function (x) { exp(x) / (1+exp(x)) }
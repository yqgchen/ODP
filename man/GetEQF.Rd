% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetEQF.R
\name{GetEQF}
\alias{GetEQF}
\title{Compute empirical quantile functions}
\usage{
GetEQF(x, qSup, returnSup = FALSE)
}
\arguments{
\item{x}{A numeric vector holding the data.}

\item{qSup}{A numeric vector holding the support grid of the empirical quantile function.}

\item{returnSup}{Logical; indicating whether \code{sup} is to be returned. Default: \code{FALSE}.}
}
\value{
A vector holding the empirical quantile function evaluated on \code{qSup} 
if \code{returnSup} is \code{FALSE}; 
A list containing two fields \code{x} and \code{y} holding the support grid and 
the values of the empirical quantile function, respectively, 
if \code{returnSup} is \code{TRUE}.
}
\description{
For a sample of real-valued random variables \eqn{X_1,\dots,X_n}, compute the empirical quantile function 
\eqn{\hat{Q}(p) = \inf\{x: \hat{F}(x) \ge p\}}, where \eqn{\hat{F}(\cdot)} denotes 
the corresponding empirical distribution function.
}
\examples{
x <- runif(50)
edf <- GetEQF( x = x, qSup = seq(0,1,0.01), returnSup = TRUE )
}

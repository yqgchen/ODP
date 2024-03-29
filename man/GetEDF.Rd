% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetEDF.R
\name{GetEDF}
\alias{GetEDF}
\title{Compute empirical distribution functions}
\usage{
GetEDF(x, sup, returnSup = FALSE)
}
\arguments{
\item{x}{A numeric vector holding the data.}

\item{sup}{A numeric vector holding the support grid of the empirical distribution function.}

\item{returnSup}{Logical; indicating whether \code{sup} is to be returned. Default: \code{FALSE}.}
}
\value{
A vector holding the empirical distribution function evaluated on \code{sup} 
if \code{returnSup} is \code{FALSE}; 
A list containing two fields \code{x} and \code{y} holding the support grid and 
the values of the empirical distribution function, respectively, 
if \code{returnSup} is \code{TRUE}.
}
\description{
For a sample of real-valued random variables \eqn{X_1,\dots,X_n}, compute the empirical distribution function 
\eqn{\hat{F}(x) = \frac{1}{n}\sum_{i=1}^{n} \chi\{X_i\le x\}}, 
where \eqn{\chi\{\cdot\}} denotes the indicator function.
}
\examples{
x <- runif(50)
edf <- GetEDF( x = x, sup = seq(0,1,0.01), returnSup = TRUE )
}

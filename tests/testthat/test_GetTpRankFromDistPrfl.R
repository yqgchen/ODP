
test_that("Missing n do not return an error", {
  qSup <- seq(0,1,len=101)[-c(1,101)]
  expect_no_error(
    GetTpRankFromDistPrfl( qDistPrfl = list(
      x = qSup,
      ymat = t( sapply(1:5, qnorm, p = qSup) )
    ) )
  )
})

test_that("Check for Bernoulli(0.5) with input distmat", {
  n <- 4
  n0 <- n/2
  data <- matrix( c( rep(0,n0), rep(1,n-n0) ), ncol = 1 )
  newdata <- matrix( 1:4, ncol = 1 ); m <- 4
  distmat <- as.matrix( stats::dist( rbind(data,newdata) ) )[, 1:n, drop=FALSE]
  
  # true in-sample and out-of-sample cdfs and quantile functions
  qf_insample <- function (x, omega) {
    values <- abs( omega - c(0,1) )
    y <- rep( 0, length(x) )
    if ( values[1] < values[2] ) {
      y[ x > (n0-1) / (n-1) ] <- values[2]
    } else {
      y[ x > (n-n0-1) / (n-1) ] <- values[1]
    }
    y
  }
  qf_outofsample <- function (x, omega) {
    values <- abs( omega - c(0,1) )
    y <- rep( min(values), length(x) )
    if ( values[1] < values[2] ) {
      y[ x > n0 / n ] <- values[2]
    } else {
      y[ x > (n-n0) / n ] <- values[1]
    }
    y
  }
  
  
  qSup <- seq(0,1,1e-2)
  qDistPrfl <- list( 
    x = qSup, 
    ymat = t( cbind( apply( data, 1, qf_insample, x = qSup ), apply( newdata, 1, qf_outofsample, x = qSup ) ) )
  )
  res <- GetTpRankFromDistPrfl( 
    qDistPrfl = qDistPrfl,
    n = n
  )
  expect_equal( mean( abs( res[seq_len(n)] - 0.5 ) ), 0 ) # only true if n0 = n/2
  expect_true( all( diff( res[-seq_len(n)] ) < 0 ) ) # ranks for new data are in the correct order
})


test_that("Unspecified type yields an error",{
  d <- 2
  n <- 10
  data <- matrix( rnorm( n * d ), ncol = d )
  expect_error( GetDistPrfl( data = data, type = 'a'), "Unrecognized input of type." )
})

test_that("Missing both distmat and data yields an error",{
  expect_error( GetDistPrfl(), "Missing value of data and distmat: at least one of them should be specified." )
})

test_that("Specifying both newdata and distmat returns a warning",{
  d <- 2
  n <- 10
  data <- matrix( rnorm( n * d ), ncol = d )
  distmat <- as.matrix( stats::dist(data) )
  expect_warning( GetDistPrfl( distmat = distmat, data = data ),
                  "The input distmat overrides the input data when both are specified." )
})

test_that("Specifying both newdata and distmat returns an error",{
  d <- 2
  n <- 10
  m <- 5
  data <- matrix( rnorm( n * d ), ncol = d )
  distmat <- as.matrix( stats::dist(data) )
  newdata <- matrix( rnorm( m * d ), ncol = d )
  
  expect_error( GetDistPrfl( distmat = distmat, newdata = newdata ),
                "The argument newdata should only be input when the argument data is given. The input newdata is not used." )
})

test_that("Check the dimension of distmat", {
  d <- 2
  n <- 10
  m <- 5
  data <- matrix( rnorm( n * d ), ncol = d )
  newdata <- matrix( rnorm( m * d ), ncol = d )
  distmat <- as.matrix( stats::dist( rbind(data,newdata) ) )[1:n, ,drop=FALSE]
  
  expect_warning( GetDistPrfl( distmat = distmat ), "The input distmat has more columns than rows and hence is transposed.")
})

test_that("Check for Bernoulli(0.5) with input distmat", {
  n <- 4
  n0 <- n/2
  data <- matrix( c( rep(0,n0), rep(1,n-n0) ), ncol = 1 )
  newdata <- matrix( c(0,1), ncol = 1 ); m <- 2
  distmat <- as.matrix( stats::dist( rbind(data,newdata) ) )[, 1:n, drop=FALSE]
  
  # true in-sample and out-of-sample cdfs and quantile functions
  cdf_insample <- function (x, omega) {
    values <- abs( omega - c(0,1) )
    y <- rep( 0, length(x) )
    if ( values[1] < values[2] ) {
      y[ x >= values[1] ] <- (n0-1) / (n-1)
    } else {
      y[ x >= values[2] ] <- (n-n0-1) / (n-1)
    }
    y[ x >= max(values) ] <- 1
    y
  }
  cdf_outofsample <- function (x, omega) {
    values <- abs( omega - c(0,1) )
    y <- rep( 0, length(x) )
    if ( values[1] < values[2] ) {
      y[ x >= values[1] ] <- n0 / n
    } else {
      y[ x >= values[2] ] <- (n-n0) / n
    }
    y[ x >= max(values) ] <- 1
    y
  }
  qf_insample <- function (x, omega) {
    values <- abs( omega - c(0,1) )
    y <- rep( min(values), length(x) )
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
  
  res <- GetDistPrfl( distmat = distmat, type = 'all' )
  
  for ( i in 1:n ) {
    # check if cdf support has right coverage
    expect_equal( any( res$cdf[[i]]$x < 0 ), TRUE ) 
    expect_equal( any( res$cdf[[i]]$x > 1 ), TRUE )
    
    # check the values of in-sample cdfs
    expect_equal( res$cdf[[i]]$y, cdf_insample(res$cdf[[i]]$x, data[i,]) )
  }
  for ( i in seq_len(m)+n ) {
    # check if cdf support has right coverage
    expect_equal( any( res$cdf[[i]]$x < 0 ), TRUE ) 
    expect_equal( any( res$cdf[[i]]$x > 1 ), TRUE )
    
    # check the values of out-of-sample cdfs
    expect_equal( res$cdf[[i]]$y, cdf_outofsample(res$cdf[[i]]$x, newdata[i-n,]) )
  }
  
  for ( i in 1:n ) {
    # check the values of in-sample quantile functions
    expect_equal( res$qf$ymat[i,], qf_insample(res$qf$x, data[i,]) )
  }
  for ( i in seq_len(m)+n ) {
    # check the values of out-of-sample quantile functions
    expect_equal( res$qf$ymat[i,], qf_outofsample(res$qf$x, newdata[i-n,]) )
  }
  
  expect_equal( res$n, n )
  
})

test_that("Check for Bernoulli(0.5) with input data and newdata", {
  n <- 4
  n0 <- n/2
  data <- matrix( c( rep(0,n0), rep(1,n-n0) ), ncol = 1 )
  newdata <- matrix( c(0,1), ncol = 1 ); m <- 2
  distmat <- as.matrix( stats::dist( rbind(data,newdata) ) )[, 1:n, drop=FALSE]
  
  # true in-sample and out-of-sample cdfs and quantile functions
  cdf_insample <- function (x, omega) {
    values <- abs( omega - c(0,1) )
    y <- rep( 0, length(x) )
    if ( values[1] < values[2] ) {
      y[ x >= values[1] ] <- (n0-1) / (n-1)
    } else {
      y[ x >= values[2] ] <- (n-n0-1) / (n-1)
    }
    y[ x >= max(values) ] <- 1
    y
  }
  cdf_outofsample <- function (x, omega) {
    values <- abs( omega - c(0,1) )
    y <- rep( 0, length(x) )
    if ( values[1] < values[2] ) {
      y[ x >= values[1] ] <- n0 / n
    } else {
      y[ x >= values[2] ] <- (n-n0) / n
    }
    y[ x >= max(values) ] <- 1
    y
  }
  qf_insample <- function (x, omega) {
    values <- abs( omega - c(0,1) )
    y <- rep( min(values), length(x) )
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
  
  res <- GetDistPrfl( data = data, newdata = newdata, type = 'all' )
  
  for ( i in 1:n ) {
    # check if cdf support has right coverage
    expect_equal( any( res$cdf[[i]]$x < 0 ), TRUE ) 
    expect_equal( any( res$cdf[[i]]$x > 1 ), TRUE )
    
    # check the values of in-sample cdfs
    expect_equal( res$cdf[[i]]$y, cdf_insample(res$cdf[[i]]$x, data[i,]) )
  }
  for ( i in seq_len(m)+n ) {
    # check if cdf support has right coverage
    expect_equal( any( res$cdf[[i]]$x < 0 ), TRUE ) 
    expect_equal( any( res$cdf[[i]]$x > 1 ), TRUE )
    
    # check the values of out-of-sample cdfs
    expect_equal( res$cdf[[i]]$y, cdf_outofsample(res$cdf[[i]]$x, newdata[i-n,]) )
  }
  
  for ( i in 1:n ) {
    # check the values of in-sample quantile functions
    expect_equal( res$qf$ymat[i,], qf_insample(res$qf$x, data[i,]) )
  }
  for ( i in seq_len(m)+n ) {
    # check the values of out-of-sample quantile functions
    expect_equal( res$qf$ymat[i,], qf_outofsample(res$qf$x, newdata[i-n,]) )
  }
  
  expect_equal( res$n, n )
  
  expect_equal( sum( abs( res$distmat - distmat ) ), 0 )
})


test_that("Unspecified type yields an error and does not otherwise",{
  d <- 2
  n <- 10
  data <- matrix( rnorm( n * d ), ncol = d )
  expect_error( GetTpRank( data = data, type = 'cdf'), "Wrong input of type." )
  GetTpRank( data = data, type = 'qf')
  GetTpRank( data = data, type = 'all')
})

test_that("Missing both distmat and data yields an error",{
  expect_error( GetTpRank(), "Missing value of data and distmat: at least one of them should be specified." )
})

test_that("Specifying both newdata and distmat returns a warning",{
  d <- 2
  n <- 10
  data <- matrix( rnorm( n * d ), ncol = d )
  distmat <- as.matrix( stats::dist(data) )
  expect_warning( GetTpRank( distmat = distmat, data = data ),
                  "The input distmat overrides the input data when both are specified." )
})

test_that("Specifying both newdata and distmat returns an error",{
  d <- 2
  n <- 10
  m <- 5
  data <- matrix( rnorm( n * d ), ncol = d )
  distmat <- as.matrix( stats::dist(data) )
  newdata <- matrix( rnorm( m * d ), ncol = d )
  
  expect_error( GetTpRank( distmat = distmat, newdata = newdata ),
                "The argument newdata should only be input when the argument data is given. The input newdata is not used." )
})

test_that("Check the dimension of distmat", {
  d <- 2
  n <- 10
  m <- 5
  data <- matrix( rnorm( n * d ), ncol = d )
  newdata <- matrix( rnorm( m * d ), ncol = d )
  distmat <- as.matrix( stats::dist( rbind(data,newdata) ) )[1:n, ,drop=FALSE]
  
  expect_warning( GetTpRank( distmat = distmat ), "The input distmat has more columns than rows and hence is transposed.")
})

test_that("Check for Bernoulli(0.5) with input distmat", {
  n <- 4
  n0 <- n/2
  data <- matrix( c( rep(0,n0), rep(1,n-n0) ), ncol = 1 )
  newdata <- matrix( 1:4, ncol = 1 ); m <- 4
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
  
  res <- GetTpRank( distmat = distmat, type = 'all' )
  
  for ( i in 1:n ) {
    # check if cdf support has right coverage
    expect_equal( any( res$profile$cdf[[i]]$x < 0 ), TRUE ) 
    expect_equal( any( res$profile$cdf[[i]]$x > 1 ), TRUE )
    
    # check the values of in-sample cdfs
    expect_equal( res$profile$cdf[[i]]$y, cdf_insample(res$profile$cdf[[i]]$x, data[i,]) )
  }
  for ( i in seq_len(m)+n ) {
    # check if cdf support has right coverage
    expect_equal( any( res$profile$cdf[[i]]$x < min(abs(newdata[i-n]-c(0,1))) ), TRUE ) 
    expect_equal( any( res$profile$cdf[[i]]$x > max(abs(newdata[i-n]-c(0,1))) ), TRUE )
    
    # check the values of out-of-sample cdfs
    expect_equal( res$profile$cdf[[i]]$y, cdf_outofsample(res$profile$cdf[[i]]$x, newdata[i-n,]) )
  }
  
  for ( i in 1:n ) {
    # check the values of in-sample quantile functions
    expect_equal( res$profile$qf$ymat[i,], qf_insample(res$profile$qf$x, data[i,]) )
  }
  for ( i in seq_len(m)+n ) {
    # check the values of out-of-sample quantile functions
    expect_equal( res$profile$qf$ymat[i,], qf_outofsample(res$profile$qf$x, newdata[i-n,]) )
  }
  
  expect_equal( res$n, n )
  
  expect_equal( mean( abs( res$rank[seq_len(n)] - 0.5 ) ), 0 ) # only true if n0 = n/2
  expect_true( all( diff( res$rank[-seq_len(n)] ) < 0 ) ) # ranks for new data are in the correct order
  
  
})

test_that("Check for Bernoulli(0.5) with input data and newdata", {
  n <- 4
  n0 <- n/2
  data <- matrix( c( rep(0,n0), rep(1,n-n0) ), ncol = 1 )
  newdata <- matrix( 1:4, ncol = 1 ); m <- 4
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
  
  res <- GetTpRank( data = data, newdata = newdata, type = 'all' )
  
  for ( i in 1:n ) {
    # check if cdf support has right coverage
    expect_equal( any( res$profile$cdf[[i]]$x < 0 ), TRUE ) 
    expect_equal( any( res$profile$cdf[[i]]$x > 1 ), TRUE )
    
    # check the values of in-sample cdfs
    expect_equal( res$profile$cdf[[i]]$y, cdf_insample(res$profile$cdf[[i]]$x, data[i,]) )
  }
  for ( i in seq_len(m)+n ) {
    # check if cdf support has right coverage
    expect_equal( any( res$profile$cdf[[i]]$x < min(abs(newdata[i-n]-c(0,1))) ), TRUE ) 
    expect_equal( any( res$profile$cdf[[i]]$x > max(abs(newdata[i-n]-c(0,1))) ), TRUE )
    
    # check the values of out-of-sample cdfs
    expect_equal( res$profile$cdf[[i]]$y, cdf_outofsample(res$profile$cdf[[i]]$x, newdata[i-n,]) )
  }
  
  for ( i in 1:n ) {
    # check the values of in-sample quantile functions
    expect_equal( res$profile$qf$ymat[i,], qf_insample(res$profile$qf$x, data[i,]) )
  }
  for ( i in seq_len(m)+n ) {
    # check the values of out-of-sample quantile functions
    expect_equal( res$profile$qf$ymat[i,], qf_outofsample(res$profile$qf$x, newdata[i-n,]) )
  }
  
  expect_equal( res$n, n )
  
  expect_equal( sum( abs( res$distmat - distmat ) ), 0 )
  
  expect_equal( mean( abs( res$rank[seq_len(n)] - 0.5 ) ), 0 ) # only true if n0 = n/2
  expect_true( all( diff( res$rank[-seq_len(n)] ) < 0 ) ) # ranks for new data are in the correct order
  
})

test_that("Check for null case",{
  n <- m <- 50
  data <- rep( rnorm(n,0,1), 2 )
  distmat <- abs( matrix( data, nr = n+m, nc = n+m ) - matrix( data, nr = n+m, nc = n+m, byrow = TRUE ) )
  
  stat <- GetPrflHomTestStat( distmat = distmat, n = n )
  expect_type( stat, 'double' )
})

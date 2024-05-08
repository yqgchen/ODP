test_that("Check for null case",{
  n <- m <- 50
  data <- rep( rnorm(n,0,1), 2 )
  distmat <- abs( matrix( data, nr = n+m, nc = n+m ) - matrix( data, nr = n+m, nc = n+m, byrow = TRUE ) )
  
  pval <- prflHomTest( distmat = distmat, n = n, nPerm = 999 )
  expect_type( pval, 'double' )
  expect_gt( pval, 0.95 )
})


test_that("Wrong format of data does return an error", {
  expect_error( GetPairDist( data = c(0,0,1) ), "wrong format of data" )
  expect_error( GetPairDist( data = diag(2), newdata = c(0,1) ), "wrong format of newdata" )
})

test_that("Data of size 1 do return an error", {
  expect_error( GetPairDist( data = matrix( c(0,0), nrow = 1 ) ), "The sample size cannot be less than 2." )
})

test_that("Check for data being an identity matrix", {
  dim <- 4
  data <- diag(dim)
  true_distmat <- matrix( sqrt(2), nrow = dim, ncol = dim )
  diag(true_distmat) <- 0
  expect_equal( GetPairDist( data = data ), true_distmat )
})

test_that("Check for data 1-dim. normal distributions with mean shift", {
  mu <- 1:4
  qSup <- seq(0,1,len=1001)[-c(1,1001)]
  data <- lapply( mu, qnorm, p = qSup )
  true_distmat <- as.matrix( dist( matrix(mu, ncol = 1) ) )
  expect_lt( mean( abs( GetPairDist( data = data, distfun = l2metric, sup = qSup ) - true_distmat ) ), 2e-3 )
})

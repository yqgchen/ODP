
test_that("Check for given data",{
  data <- (1:10) / 10
  sup <- c(-0.1,0.12,0.2,0.98,1,1.1)
  res <- GetEDF( x = data, sup = sup, returnSup = FALSE )
  res_true <- c(0,1,2,9,10,10)/10
  expect_equal( max( abs( res - res_true ) ), 0 )
})

test_that("Check the format of output when returnSup is FALSE",{
  data <- (1:10) / 10
  sup <- c(-0.1,0.12,0.2,0.98,1,1.1)
  res <- GetEDF( x = data, sup = sup, returnSup = FALSE )
  expect_true( is.vector(res) )
  expect_true( is.numeric(res) )
  expect_equal( length(res), length(sup) )
})

test_that("Check the format of output when returnSup is TRUE",{
  data <- (1:10) / 10
  sup <- c(-0.1,0.12,0.2,0.98,1,1.1)
  res <- GetEDF( x = data, sup = sup, returnSup = TRUE )
  expect_true( is.list(res) )
  expect_equal( length(res), 2 )
  expect_equal( names(res), c('x','y') )
  expect_true( is.numeric(res$y) )
  expect_equal( length(res$y), length(sup) )
  expect_equal( length(res$x), length(sup) )
})

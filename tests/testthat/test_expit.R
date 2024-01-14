
test_that('expit yields the right value', {
  expect_equal( expit(1), exp(1)/(1+exp(1)) )
  expect_equal( expit(0), 1/2 )
})

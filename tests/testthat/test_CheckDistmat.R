#require(testthat)

test_that("non-square matrix with a symmetric upper square sub-matrix and zero diagonals does not return an error",{
  A <- rbind(matrix(1,ncol=3,nrow=3) - diag(3), 1:3)
  expect_no_error( CheckDistmat(A) )
})

test_that("symmetric square matrix with zero diagonals does not return an error",{
  A <- matrix(1,ncol=3,nrow=3) - diag(3)
  expect_no_error( CheckDistmat(A) )
})

test_that("non-square matrix with an asymmetric upper square sub-matrix does return an error",{
  A <- matrix(1:12, ncol=3)
  diag(A) <- 0
  expect_error(
    CheckDistmat(A),
    "The input distmat or its upper square sub-matrix is not symmetric."
  )
})

test_that("asymmetric square matrix does return an error",{
  A <- matrix(1:9, ncol=3)
  diag(A) <- 0
  expect_error(
    CheckDistmat(A),
    "The input distmat or its upper square sub-matrix is not symmetric."
  )
})

test_that("non-square matrix with non-zero diagonals does return an error",{
  A <- rbind(matrix(1, ncol = 3, nrow = 3), 1:3)
  expect_error(
    CheckDistmat(A),
    "The input distmat has non-zero diagonals."
  )
})

test_that("square matrix with non-zero diagonals does return an error",{
  A <- matrix(1, ncol = 3, nrow = 3)
  expect_error(
    CheckDistmat(A),
    "The input distmat has non-zero diagonals."
  )
})


save_png <- function(code, width = 4, height = 4) {
  path <- tempfile(fileext = ".png")
  code
  ggsave( filename = path, width = width, height = height )
  
  path
}

expect_snapshot_plot <- function(name, code, width = 4, height = 4 ) {
  # Other packages might affect results
  skip_if_not_installed("ggplot2", "2.0.0")
  # Or maybe the output is different on some operation systems
  skip_on_os("windows")
  # You'll need to carefully think about and experiment with these skips
  
  name <- paste0(name, ".png")
  
  # Announce the file before touching `code`. This way, if `code`
  # unexpectedly fails or skips, testthat will not auto-delete the
  # corresponding snapshot file.
  announce_snapshot_file(name = name)
  
  path <- save_png(code, width = width, height = height)
  expect_snapshot_file(path, name)
}

test_that("2-dim Gaussian case", {
  n <- 500
  p <- 2
  set.seed(1)
  data <- matrix( rnorm( n * p ), ncol = p )
  res <- GetTpRank( data = data )
  
  local_edition(3)
  expect_snapshot_plot(
    'test_2dGaussian',
    MakePrflMdsPlot( qDistPrfl = res$profile$qf, color_by = res$rank, nGroup = 10, colorlab = 'Rank group' ) 
  )
})

test_that("mortality 2000 male", {
  load( system.file('testdata', 'mort.Rda', package='ODP')  )
  res <- GetTpRank( data = mort$qt, distfun = l2metric, sup = mort$qSup )
  
  local_edition(3)
  expect_snapshot_plot(
    'test_mort2000male',
    MakePrflMdsPlot( qDistPrfl = res$profile$qf, color_by = res$rank, nGroup = 10,
                     id = rownames(mort$qt), colorlab = 'Rank group' ),
    width = 6, height = 4
  )
})

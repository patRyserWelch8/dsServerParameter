context("createMatrixRUnifDS::expt::correct_parameters")
test_that("correct number of values",
{
  outcome <- createMatrixRUnifDS()
  expect_equal(nrow(outcome), 11)
  expect_equal(ncol(outcome), 13)
  expect_equal(identical(outcome[outcome[,] < -10^16],numeric(0)), TRUE)
  expect_equal(identical(outcome[outcome[,] > 10^16],numeric(0)), TRUE)

  outcome <- createMatrixRUnifDS(11,13)
  expect_equal(nrow(outcome), 11)
  expect_equal(ncol(outcome), 13)
  expect_equal(identical(outcome[outcome[,] < -10^16],numeric(0)), TRUE)
  expect_equal(identical(outcome[outcome[,] > 10^16],numeric(0)), TRUE)
})


context("createMatrixRUnifDS::expt::incorrect_parameters")
test_that("incorrect parameters",
{
   outcome <- createMatrixRUnifDS("11","13")
  expect_equal (is.null(outcome), TRUE)
})


context("occultVectorMatrixDS::expt::incorrect_parameters")
test_that("incorrect_paramters",
{
  concealing.matrix <- occultVectorMatrixDS()
  expect_equal(sum(concealing.matrix),0)
  expect_equal(nrow(concealing.matrix),2)
  expect_equal(ncol(concealing.matrix),2)

  concealing.matrix <- occultVectorMatrixDS("aVectorDoesNotExist")
  expect_equal(sum(concealing.matrix),0)
  expect_equal(nrow(concealing.matrix),2)
  expect_equal(ncol(concealing.matrix),2)

  concealing.matrix <- occultVectorMatrixDS("aVectorDoesNotExist","aMatrixDoesNotExist")
  expect_equal(sum(concealing.matrix),0)
  expect_equal(nrow(concealing.matrix),2)
  expect_equal(ncol(concealing.matrix),2)

})

context("occultVectorMatrixDS::expt::incorrect_length")
test_that("incorrect_length",
{
  ds.test_env$vector <- c(1,2,3,4)
  ds.test_env$concealing.matrix <- matrix(1:25,nrow = 5, ncol = 5)
  outcome <- occultVectorMatrixDS("vector","concealing.matrix",ds.test_env)
  expect_equal(sum(outcome),4)
  expect_equal(nrow(outcome),2)
  expect_equal(ncol(outcome),2)

  ds.test_env$vector <- c(1,2,3,4,5)
  ds.test_env$concealing.matrix <- matrix(1:16, nrow = 4, ncol = 4)
  print(ls())
  outcome <- occultVectorMatrixDS("vector","concealing.matrix",ds.test_env)
  expect_equal(sum(outcome),4)
  expect_equal(nrow(outcome),2)
  expect_equal(ncol(outcome),2)


})


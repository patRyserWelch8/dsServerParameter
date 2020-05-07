source('definition_tests/def-createRUnifWrapperDS.R')

context("createMatrixRUnifWrapperDS::expt::incorrect_parameters")
test_that("incorrect parameters",
{
  .test_failure_size_parameter("123","789")
  .test_failure_size_parameter(TRUE,TRUE)
  .test_failure_size_parameter(FALSE,FALSE)
  .test_failure_size_parameter(c(1,2,2),c(1,2,2))
  .test_failure_all_parameters(123,123,"new.matrix",globalenv())

})

context("createMatrixRUnifWrapperDS::expt::correct_parameters")
test_that("correct number of values",
{
  .test_success_no_param()
  .test_success_size_parameter(11,13)
  .test_success_size_parameter(c(11),c(13))
  .test_success_size_parameter(100,200)
  .test_success_all_parameters(11,13,"new.matrix.1")
  .test_success_all_parameters(110,130,"new.matrix.2")
})






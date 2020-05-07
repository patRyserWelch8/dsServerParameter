source("definition_tests/def_sharing_structure.R")
source("definition_tests/def_getEncodedDataDS.R")

context("getEncodedDataDS::expt::exists")
test_that("variables does not exist",
{
  data <- getEncodedDataDS()
  .test.data.structure(data)
  expect_error(.encode.data.with.sharing())
  .test.data.structure(.encode.data.no.sharing())
})

context("getEncodedDataDS::expt::")
test_that("variables exists",
{
  .test_sharing_is_created()
  
})

context("getEncodedDataDS::expt::exists")
test_that("variables exists",
{
  initiateExchangeDS()
  data <- getEncodedDataDS()
  .test.data.structure(data)
  .test.data.structure(.encode.data.no.sharing())
  .test.data.structure(.encode.data.with.sharing())
})


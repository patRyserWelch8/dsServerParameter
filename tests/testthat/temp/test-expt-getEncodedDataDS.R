source("definition_tests/def_sharing_structure.R")
source("definition_tests/def_getEncodedDataDS.R")

rm("sharing", pos=1)

context("getEncodedDataDS::expt::exists")
test_that("variables does not exist",
{
  expect_error(.encode.data.with.sharing())
  .test.data.structure(.encode.data.no.sharing())
  data <- getEncodedDataDS()
  .test.data.structure(data)
  expect_equal(data$header,"FM2")
  
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
  expect_equal(data$header,"FM1")
  .test.data.structure(data)
  .test.data.structure(.encode.data.no.sharing())
  .test.data.structure(.encode.data.with.sharing())
})


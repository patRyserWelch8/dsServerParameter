source("definition_tests/def_getEncodedDataDS.R")
source('definition_tests/def_sendEncodedDataDS.R')

initiateExchangeDS()
data <- getEncodedDataDS()

context("sendEncodedDataDS::expt::parameters")
test_that("parameters",
{
 
  expect_equal(sendEncodedDataDS(), FALSE)
  expect_equal(sendEncodedDataDS(1), FALSE)
  expect_equal(sendEncodedDataDS("FM1",TRUE), FALSE)
  expect_equal(sendEncodedDataDS("FM1","123,123","WRONG"), FALSE)
  expect_equal(sendEncodedDataDS("FM1","123,123",1,"WRONG" ), FALSE)
  expect_equal(sendEncodedDataDS("FM1","123,123",1,13,"WRING" ), FALSE)
  expect_equal(sendEncodedDataDS("FM1","123,123",1,13,2.3,"INCORRECT" ), FALSE)
 
  
})

context("sendEncodedDataDS::expt::")
test_that("variables exists",
{
  .test.data.structure(data)
  print(length(data$header))
  result  <- sendEncodedDataDS(data$header, data$payload, data$property.a, data$property.b, data$property.c, data$property.d)
  sharing <- get("sharing", pos=1)
  expect_equal("received.matrix" %in% names(sharing),TRUE)
  expect_equal(result,TRUE)
  
})

context("sendEncodedDataDS::expt::.create.matrix")
test_that("variables exists",
{
  .test.create.matrix.parameters.correct(as.character(data$payload), data$property.b)
  .test.create.matrix.parameters.incorrect()
})

context("sendEncodedDataDS::expt::.create.matrix")
test_that("variables exists",
{
  .test.create.matrix.parameters.correct(as.character(data$payload), data$property.b)
  .test.create.matrix.parameters.incorrect()
})

context("sendEncodedDataDS::expt::.save.matrix")
test_that("variables exists",
{
  .test.save.matrix.parameters.correct()
  .test.save.matrix.parameters.incorrect()
})





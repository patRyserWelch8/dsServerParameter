source("definition_tests/def_getEncodedDataDS.R")
source('definition_tests/def_sendEncodedDataDS.R')


context("assignDataDS::expt::parameters")
test_that("parameters",
{
 
  expect_equal(assignDataDS(), FALSE)
  expect_equal(assignDataDS(1), FALSE)
  expect_equal(assignDataDS("FM1",TRUE), FALSE)
  expect_equal(assignDataDS("FM1","123,123","WRONG"), FALSE)
  expect_equal(assignDataDS("FM1","123,123",1,"WRONG" ), FALSE)
  expect_equal(assignDataDS("FM1","123,123",1,13,"WRING" ), FALSE)
  expect_equal(assignDataDS("FM1","123,123",1,13,2.3,"INCORRECT" ), FALSE)
 
  
})

pi_value = 1000
assignSharingSettingsDS()


encryptDataDS(TRUE, FALSE)
master.1 <- get("sharing",pos=1)

a <- getDataDS(master_mode =TRUE)
rm(sharing,pos=1)
result <- assignDataDS(master_mode = FALSE,a$header,a$payload,a$property.a,a$property.b,a$property.c,a$property.d)
receiver.1 <- get("sharing",pos=1)

context("assignDataDS::expt::")
test_that("variables exists",
{
  expect_equal(result,TRUE)
  .test.data.structure(a)
  sharing <- get("sharing", pos=1)
  expect_equal("received" %in% names(sharing),TRUE)
  
  
})

context("assignDataDS::expt::.create.matrix")
test_that("variables exists",
{
  .test.create.matrix.parameters.correct(as.character(a$payload), a$property.b)
  .test.create.matrix.parameters.incorrect()
})

context("assignDataDS::expt::.create.matrix")
test_that("variables exists",
{
  .test.create.matrix.parameters.correct(as.character(a$payload), a$property.b)
  .test.create.matrix.parameters.incorrect()
})

context("assignDataDS::expt::.save.matrix")
test_that("variables exists",
{
  .test.save.matrix.parameters.correct()
  .test.save.matrix.parameters.incorrect()
})





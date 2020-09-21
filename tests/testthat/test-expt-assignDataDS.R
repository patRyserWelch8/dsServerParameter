source("definition_tests/def_getEncodedDataDS.R")
source('definition_tests/def_sendEncodedDataDS.R')


context("assignDataDS::expt:: incorrect parameters")
test_that("parameters",
{
  expect_error(assignDataDS(),"SERVER::ERR::PARAM::002")
  expect_error(assignDataDS(1), "SERVER::ERR::PARAM::002")
  expect_error(assignDataDS("FM1",TRUE), "SERVER::ERR::PARAM::002")
  expect_error(assignDataDS("FM1","123,123","WRONG"),"SERVER::ERR::PARAM::002")
  expect_error(assignDataDS("FM1","123,123",1,"WRONG" ), "SERVER::ERR::PARAM::002")
  expect_error(assignDataDS("FM1","123,123",1,13,"WRING" ), "SERVER::ERR::PARAM::002")
  expect_error(assignDataDS("FM1","123,123",1,13,2.3,"INCORRECT" ), "SERVER::ERR::PARAM::002")
})

options(param.name.struct = "sharing")
options(param.sharing.allowed = 1) 

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



context("assignDataDS::expt::.save.matrix")
test_that("variables exists",
{
  .test.save.matrix.parameters.correct()
  .test.save.matrix.parameters.incorrect()
})





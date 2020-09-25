source("definition_tests/def_getEncodedDataDS.R")

rm("sharing", pos=1)
rm("settings", pos=1)

context("getDataDS::expt::no_settings")
test_that("no_settings",
{
  expect_error(getDataDS(master_mode = TRUE), "SERVER::ERR::PARAM::001")
})

options(param.name.struct = "sharing")
options(param.sharing.allowed = 0) 
assignSharingSettingsDS()

context("getDataDS::expt::no_sharing")
test_that("no_sharing",
{
  expect_error(getDataDS(), "SERVER::ERR::PARAM::001")
  assign("sharing", list(), pos = 1)
  expect_true(is.list(.encode.encrypted.data())) 
})


options(param.name.struct = "sharing")
options(param.sharing.allowed = 1) 

#("Step 0")
pi_value_1 = 100000
pi_value_2 = 200000
pi_value_3 = 300000
assignSharingSettingsDS()

#print("Step 1")
encryptDataDS(TRUE, FALSE)
master.1 <- get("sharing",pos=1)


context("getDataDS::expt::")
test_that("variables exists",
{
   data <- getDataDS()
  .test.data.structure(data)
  expect_equal(data$header,"FM1")
  .test.data.structure(data)
  .test.data.structure(encode.data.no.sharing())
  .test.data.structure(encode.data.with.sharing(master.1$encrypted,ncol(master.1$encrypted),15))
})


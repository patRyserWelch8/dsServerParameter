source("definition_tests/def_getEncodedDataDS.R")

rm("sharing", pos=1)
rm("settings", pos=1)

context("getDataDS::expt::no_settings")
test_that("no_settings",
{
  expect_error(getDataDS(master_mode = TRUE), "SERVER::ERR::PARAM::002")
})

assignSharingSettingsDS()

context("getDataDS::expt::no_sharing")
test_that("no_sharing",
{
  expect_error(getDataDS(), "SERVER::ERR::PARAM::003")
  assign("sharing", list(), pos = 1)
  expect_error(.encode.encrypted.data(),"SERVER::ERR::PARAM::004")
})

encryptDataDS(TRUE, FALSE)
master.1 <- get("sharing",pos=1)

context("getDataDS::expt::")
test_that("variables exists",
{
  data <- getDataDS()
  .test.data.structure(data)
  expect_equal(data$header,"FM1")
  .test.data.structure(data)
  .test.data.structure(.encode.data.no.sharing())
  .test.data.structure(.encode.data.with.sharing(master.1$encrypted,ncol(master.1$encrypted),15))
})


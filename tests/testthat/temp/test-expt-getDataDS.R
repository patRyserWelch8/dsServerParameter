source("definition_tests/def_getEncodedDataDS.R")

rm("sharing", pos=1)
rm("settings", pos=1)

context("getDataDS::expt::no_settings")
test_that("no_settings",
{
  data <- getDataDS()
  .test.data.structure(data)
  expect_equal(data$header,"FM3")
  
})

assignSharingSettingsDS()

context("getDataDS::expt::no_sharing")
test_that("no_sharing",
{
  data <- getDataDS()
  .test.data.structure(data)
  expect_equal(data$header,"FM2")
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


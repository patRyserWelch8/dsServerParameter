
if(exists("settings",where = 1))
{
  rm("settings",pos=1)
}


context("removeExchangeDataDS::expt::no_settings")
test_that("no_setting",
{
  expect_equal(exists("settings",where = 1),FALSE)
  expect_equal(removeExchangeDataDS(),TRUE)
})

assign("settings", list(), pos = 1 )

context("removeExchangeDataDS::expt::with_settings")
test_that("with_setting",
{
  expect_equal(exists("settings",where = 1),TRUE)
  expect_equal(removeExchangeDataDS(),TRUE)
  assign("settings", list(name.struct = "sharing"), pos = 1 )
  expect_equal(removeExchangeDataDS(),TRUE)
})

assign("settings", list(name.struct = "sharing"), pos = 1 )
assign("sharing", list(), pos = 1)

context("removeExchangeDataDS::expt::with_sharing")
test_that("with_sharing",
{
  expect_equal(exists("settings",where = 1),TRUE)
  expect_equal(removeExchangeDataDS(),TRUE)
})


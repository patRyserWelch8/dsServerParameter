
rm("settings", pos = 1)
context("is.sharing.allowed::smk")
test_that("no setting",
{
  expect_false(is.sharing.allowed())
})




context("is.sharing.allowed::smk")
test_that("with setting",
{
  options(param.name.struct = "sharing")
  options(param.sharing.allowed = 0) 
  assignSharingSettingsDS()
  expect_false(is.sharing.allowed())
})



context("is.sharing.allowed::smk")
test_that("with setting and allowed",
{
  options(param.name.struct = "sharing")
  options(param.sharing.allowed = 1) 
  assignSharingSettingsDS()
  expect_true(is.sharing.allowed())
})



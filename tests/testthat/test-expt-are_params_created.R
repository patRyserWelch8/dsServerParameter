
context("are_params_created::smk")
test_that("call",
{
  expect_equal(are.params.created(c("hi")), FALSE)
})


context("are_params_created::expt::correct")
test_that("all correct arguments",
{
  assign("a", 1, pos = 1)
  expect_equal(are.params.created(c("a")), TRUE)
  assign("b", "hi", pos = 1)
  expect_equal(are.params.created(c("b")), FALSE)
  assign("c", 123.3, pos = 1)
  expect_equal(are.params.created(c("a", "b", "c")), FALSE)
  expect_equal(are.params.created(c("a", "d", "c")), FALSE)
  assign("d", -123.3, pos = 1)
  expect_equal(are.params.created(c("a", "d", "c")), TRUE)
})


context("are_params_created::expt::incorrect")
test_that("incorrect arguments",
{
  expect_equal(are.params.created(c(1)), FALSE)
  expect_equal(are.params.created(c(1,2,3)), FALSE)
  expect_equal(are.params.created(c(1, "a")), FALSE)
  expect_equal(are.params.created(c("a", TRUE)), FALSE)
})

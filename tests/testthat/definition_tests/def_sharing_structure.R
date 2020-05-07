
.test_sharing_is_created <- function()
{
  expected.list <- c("concealing.matrix","encoded.matrix","masking.matrix","master.vector")
  #test no sharing environment variable exists
  expect_error(get("sharing", envir=globalenv()))
  initiateExchangeDS()
  expect_equal(is.list(get("sharing", envir=globalenv())),TRUE)
  expect_equal(all(expected.list %in% names(sharing), TRUE), TRUE)
  expect_equal(length(sharing) == length(expected.list), TRUE)
  expect_equal(is.vector(sharing$master.vector), TRUE)
  expect_equal(is.matrix(sharing$encoded.matrix), TRUE)
  expect_equal(is.matrix(sharing$masking.matrix), TRUE)
  expect_equal(is.matrix(sharing$concealing.matrix), TRUE)
}


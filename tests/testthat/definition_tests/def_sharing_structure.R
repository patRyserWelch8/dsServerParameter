
expected.list <- c("concealing","encrypted","masking")

.test_sharing_is_created <- function()
{
  #test no sharing environment variable exists
  if (exists("sharing", where=1))
  {
    rm(list = "sharing", pos=1)
  }
  
  expect_error(get("sharing", pos=1))
  success <- encryptDataDS(master_mode = TRUE, preserve_mode = FALSE)
  
}

if(FALSE)
{

.test_sharing_receiver <- function()
{
  #simulate an secure exchange of date of phase I and II of the algorithm
  initiateExchangeDS(master = TRUE)
  data <- getEncodedDataDS()
  sendEncodedDataDS(data$header, data$payload, data$property.a, data$property.b,
                    data$property.c, data$property.d)
  
  #test
  initiateExchangeDS(FALSE)
  expect_equal(is.list(get("sharing", envir=globalenv())),TRUE)
  expect_equal(all(expected.list %in% names(sharing), TRUE), TRUE)
  expect_equal(length(sharing) >= length(expected.list), TRUE)
  expect_equal(is.vector(sharing$master.vector), TRUE)
  expect_equal(is.matrix(sharing$encoded.matrix), TRUE)
  expect_equal(is.matrix(sharing$masking.matrix), TRUE)
  expect_equal(is.matrix(sharing$concealing.matrix), TRUE)
}

}

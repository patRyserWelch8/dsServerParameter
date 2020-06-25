
context("removeEncryptingDataDS::expt::no_settings")
test_that("no_setting",
{
  if(exists("settings",where = 1))
  {
    rm("settings",pos=1)
  }
  expect_equal(exists("settings",where = 1),FALSE)
  expect_equal(removeEncryptingDataDS(),FALSE)
  expect_error(.get.sharing.data())
})
#"Step 0"
assignSharingSettingsDS()

context("removeEncryptingDataDS::expt::get.sharing.data()")
test_that("no_sharing",
{
  if(exists("sharing",where = 1))
  {
    rm("sharing",pos=1)
  }
  expect_equal(exists("settings",where = 1),TRUE)
  expect_equal(exists("sharing",where = 1),FALSE)
  expect_equal(.get.sharing.data(),list())
})


rm(list=ls(),pos=1)
#"Step 0"
pi_value = 1000
assignSharingSettingsDS()

#("Step 1")
encryptDataDS(TRUE, FALSE)
master.1 <- get("sharing",pos=1)

#("Step 2")
a <- getDataDS(master_mode =TRUE)
rm(sharing,pos=1)
assignDataDS(master_mode = FALSE,a$header,a$payload,a$property.a,a$property.b,a$property.c,a$property.d)
receiver.1 <- get("sharing",pos=1)

#("Step 3")
encryptDataDS(FALSE, FALSE)
receiver.2 <- get("sharing",pos=1)

#("step 4")
b <- getDataDS(master_mode =  FALSE)
rm(sharing,pos=1)
assign("sharing", master.1, pos=1)
assignDataDS(master_mode = TRUE, b$header,b$payload,b$property.a,b$property.b,b$property.c,b$property.d)
master.2 <- get("sharing",pos=1)

#("step 5")
decryptDataDS()
master.3 <- get("sharing",pos=1)
encryptParamDS("pi_value")
master.4 <- get("sharing",pos=1)
removeEncryptingDataDS()
master.5 <- get("sharing",pos=1)

#("step 6 - Receiver becomes master .... ")
assign("sharing", receiver.2, pos=1)


context("removeEncryptingDataDS::expt::")
test_that("computations",
{
  expect_equal(removeEncryptingDataDS(),TRUE)
  sharing <- get("sharing", pos = 1)
  expect_equal(length(sharing),3)
  expect_equal(all(names(sharing) %in% c(settings$data,settings$no_columns, settings$no_rows)), TRUE)
})




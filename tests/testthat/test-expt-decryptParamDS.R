
context("decryptParamDS::expt::no_settings")
test_that("does not exist",
{
   if (exists("settings",where = 1))
   {
     rm("settings", pos=1)
   }
   expect_equal(exists("settings", where = 1), FALSE)
   expect_error(.get.encoded.param())
   expect_error(.is.encoded.param.valid())
   expect_equal(decryptParamDS(),FALSE)
})

assignSharingSettingsDS()
settings <- get("settings",pos=1)

context("decryptParamDS::expt::no_encryption")
test_that("does exists",
{
   if (exists("sharing",where = 1))
   {
      rm("sharing", pos=1)
   }
   expect_equal(exists("settings", where = 1), TRUE)
   expect_equal(.get.encoded.param(),list())
   expect_equal(.is.encoded.param.valid(),FALSE)
   expect_equal(decryptParamDS(),FALSE)
   expect_equal(exists(settings$name.struct, where = 1), FALSE)
})


#complete set steps to reach the point of decryption
#step 1
pi_value = 1000
assign("pi_value", pi_value, pos=1)
encryptDataDS(TRUE, FALSE)
master.1 <- get("sharing",pos=1)

#step 2
a <- getDataDS(master_mode =TRUE)
rm(sharing,pos=1)
assignDataDS(master_mode = FALSE,a$header,a$payload,a$property.a,a$property.b,a$property.c,a$property.d)
receiver.1 <- get("sharing",pos=1)

#step 3
encryptDataDS(FALSE, FALSE)
receiver.2 <- get("sharing",pos=1)

#step 4
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
removeEncryptingDataDS()
receiver.3 <- get("sharing",pos=1)
encryptDataDS(TRUE, TRUE)
receiver.4 <- get("sharing",pos=1)

#"step 7")
c <- getDataDS(master_mode = TRUE)
rm(sharing,pos=1)
assign("sharing", master.5, pos=1)
assignDataDS(master_mode = FALSE,c$header,c$payload,c$property.a,c$property.b,c$property.c,c$property.d)
master.6 <- get("sharing",pos=1)


#"step 8 ")
encryptDataDS(FALSE, TRUE)
master.7 <- get("sharing",pos=1)

#("step 9")
d <- getDataDS(master_mode = FALSE)
rm(sharing,pos=1)
assign("sharing", receiver.4, pos=1)
assignDataDS(master_mode = TRUE,d$header,d$payload,d$property.a,d$property.b,d$property.c,d$property.d)
receiver.5 <- get("sharing",pos=1)

#("step 10")
decryptDataDS()
receiver.6 <- get("sharing",pos=1)
outcome <- decryptParamDS()

context("decryptParamDS::expt::data_has_been_decrypted")
test_that("data has been encrypted correctly",
{
   expect_equal(exists(settings$name.struct, where = 1), TRUE)
   expect_equal(outcome, TRUE)
   expect_equal(exists("param", where=1), TRUE)
   expect_equal(param$param.1, pi_value)
})





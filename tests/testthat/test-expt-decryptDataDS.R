context("decryptData::expt::no_settings")
test_that("does not exist",
{
   if (exists("settings",where = 1))
   {
     rm("settings", pos=1)
   }
   expect_equal(exists("settings", where = 1), FALSE)
   expect_equal(.get_received_data(), list())
   expect_equal(.is.received.data.valid(), FALSE)
   expect_equal(.decrypt.received.matrix(), NULL)
   expect_equal(.is.decrypted.data.valid(),FALSE)
   expect_equal(decryptDataDS(),FALSE)
})

assignSharingSettingsDS()
settings <- get("settings",pos=1)

context("decryptData::expt::no_encryption")
test_that("does ex",
{
   if (exists(settings$name.struct,where = 1))
   {
      rm(settings$name.struct, pos=1)
   }
   expect_equal(exists(settings$name.struct, where = 1), FALSE)
   expect_equal(.get_received_data(), list())
   expect_equal(.is.received.data.valid(), FALSE)
   expect_equal(.decrypt.received.matrix(), NULL)
   expect_equal(.is.decrypted.data.valid(),FALSE)
   expect_equal(decryptDataDS(),FALSE)
})


#complete set steps to reach the point of decryption
#step 1

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


context("decryptData::expt::data_has_been_encrypted")
test_that("",
{
   
   
})





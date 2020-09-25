context("decryptData::expt::no_settings")
test_that("does not exist",
{
   if (exists("settings",where = 1))
   {
     rm("settings", pos=1)
   }
   expect_equal(exists("settings", where = 1), FALSE)
   
   expect_error(.get_received_data())
   expect_error(.is.received.data.valid())
   expect_equal(.decrypt.received.matrix(), NULL)
   expect_error(.is.decrypted.data.valid())
   expect_error(decryptDataDS())
})

options(param.name.struct = "sharing")
options(param.sharing.allowed = 0) 
assignSharingSettingsDS()
settings <- get("settings",pos = 1)

context("decryptData::expt::not_allowed")
test_that("not_allowed",
{
   expect_equal(exists("settings", where = 1), TRUE)
   expect_error(.is.received.data.valid())
   expect_equal(.decrypt.received.matrix(), NULL)
   expect_error(.is.decrypted.data.valid())
   expect_error(decryptDataDS(),"SERVER::ERR::PARAM::001")    
})

options(param.name.struct = "sharing")
options(param.sharing.allowed = 1) 
assignSharingSettingsDS()
settings <- get("settings",pos = 1)

context("decryptData::expt::no_encryption")
test_that("does exists",
{
   if (exists(settings$name.struct,where = 1))
   {
      rm("sharing", pos = 1)
   }
   expect_equal(exists(settings$name.struct, where = 1), FALSE)
   expect_equal(.get_received_data(), list())
   expect_error(.is.received.data.valid())
   expect_equal(.decrypt.received.matrix(), NULL)
   expect_error(.is.decrypted.data.valid())
   expect_error(decryptDataDS(),"SERVER::ERR::PARAM::007")
})


#complete set steps to reach the point of decryption
encryptDataDS(TRUE, FALSE)
assign("master.1" ,get("sharing",pos=1), pos = 1)


#("Step 2")
assign("a", getDataDS(master_mode =TRUE), pos = 1)
rm(sharing,pos=1)
assignDataDS(master_mode = FALSE,a$header,a$payload,a$property.a,a$property.b,a$property.c,a$property.d)
assign("receiver.1", get("sharing",pos=1), pos = 1)


#("Step 3")
encryptDataDS(FALSE, FALSE)
assign("receiver.2", get("sharing",pos=1), pos = 1)

#("step 4")
assign("b",getDataDS(master_mode =  FALSE), pos = 1)
rm(sharing,pos=1)
assign("sharing", get("master.1", pos = 1), pos=1)
assignDataDS(master_mode = TRUE, b$header,b$payload,b$property.a,b$property.b,b$property.c,b$property.d)
assign("master.2", get("sharing",pos=1), pos = 1)



context("decryptData::expt::data_has_been_encrypted")
test_that("data has been encrypted correctly",
{
   expect_equal(exists(settings$name.struct, where = 1), TRUE)
   expect_equal(.get_received_data(), master.2)
   expect_equal(.is.received.data.valid(master.2), TRUE)
   
   decrypted.data <- .decrypt.received.matrix(master.2$masking, master.2$received)
   rows <- nrow(decrypted.data)
   cols <- ncol(decrypted.data)
   result         <- t(master.2$concealing) %*% receiver.2$concealing 
   
   expect_equal(is.matrix(decrypted.data), TRUE)
   expect_equal(rows == cols, TRUE)
   expect_equal(decryptDataDS(),TRUE)
   expected.list  <- c(settings$received,settings$masking, settings$decrypted)
   expect_equal(.is.decrypted.data.valid(expected.list),TRUE)
})





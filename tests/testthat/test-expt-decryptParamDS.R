
context("decryptParamDS::smk::no_settings")
test_that("does not exist",
{
   rm(list=ls(pos = 1),pos=1)
   expect_equal(exists("settings", where = 1), FALSE)
   expect_error(.get.encoded.param())
   expect_error(.is.encoded.param.valid())
   expect_error(decryptParamDS())
})

context("decryptParamDS::expt::no_settings")
test_that("does not exist",
{
   rm(list=ls(pos = 1),pos=1)
   expect_equal(exists("settings", where = 1), FALSE)
   expect_error(.get.encoded.param())
   expect_error(.is.encoded.param.valid())
   expect_error(decryptParamDS(), "SERVER::ERR::PARAM::001")
   
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
   expect_error(decryptParamDS(),"SERVER::ERR::PARAM::009")
   expect_equal(exists(settings$name.struct, where = 1), FALSE)
})


#complete set steps to reach the point of decryption
#("Step 0")
rm(list=ls(pos = 1),pos=1)
options(param.name.struct = "sharing")
options(param.sharing.allowed = 1) 

#("Step 0")
assign("pi_value_1", 100.523, pos = 1)
assign("pi_value_2", 200.654, pos = 1)
assign("pi_value_3", 300.789, pos = 1)

assignSharingSettingsDS()


#("Step 1")
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


#("step 5")
decryptDataDS()
assign("master.3", get("sharing",pos=1), pos = 1)
outcome <- assignParamSettingsDS(c("pi_value_1","pi_value_2","pi_value_3"))
assign("master.3.5", get("sharing",pos = 1), pos = 1)
assign("f", getCoordinatesDS(), pos = 1)
rm(sharing,pos=1)
assign("sharing", get("receiver.2", pos = 1), pos=1)
assignCoordinatesDS(f$header, f$payload,f$property.a,f$property.b,f$property.c,f$property.d)
assign("receiver.2.5", get("sharing", pos = 1), pos = 1)
rm(sharing,pos=1)
assign("sharing", get("master.3.5", pos = 1), pos=1)

encryptParamDS()

assign("master.4", get("sharing",pos = 1), pos = 1)
removeEncryptingDataDS(master_mode = TRUE)
assign("master.5", get("sharing",pos = 1), pos = 1)

#("step 6 - Receiver becomes master .... ")
assign("sharing", get("receiver.2.5", pos = 1), pos=1)
removeEncryptingDataDS(master_mode = FALSE)
assign("receiver.3", get("sharing",pos = 1), pos = 1)
assign("outcome", encryptDataDS(TRUE, TRUE), pos = 1)
assign("receiver.4", get("sharing",pos = 1), pos = 1)

#("step 7")
assign("c", getDataDS(master_mode = TRUE), pos = 1)
rm(sharing,pos=1)
assign("sharing", master.5, pos = 1)
c <- get("c" ,pos = 1)
assignDataDS(master_mode = FALSE,c$header,c$payload,c$property.a,c$property.b,c$property.c,c$property.d)
assign("master.6", get("sharing",pos = 1), pos = 1)

#("step 8 ")
encryptDataDS(FALSE, TRUE)
assign("master.7", get("sharing",pos=1), pos = 1)

#("step 9")
assign("d", getDataDS(master_mode = FALSE), pos = 1)
rm(sharing,pos=1)
assign("sharing", get("receiver.4", pos = 1), pos=1)
d <- get("d", pos = 1)
assignDataDS(master_mode = TRUE,d$header,d$payload,d$property.a,d$property.b,d$property.c,d$property.d)
assign("receiver.5", get("sharing",pos = 1), pos = 1)


#("step 10")
decryptDataDS()
assign("receiver.6", get("sharing",pos=1), pos = 1)
assign("outcome", decryptParamDS(c("pi_value_1_a", "pi_value_2_a", "pi_value_3_a"),3), pos = 1)

context("decryptParamDS::expt::data_has_been_decrypted")
test_that("data has been encrypted correctly",
{
   expect_equal(exists(settings$name.struct, where = 1), TRUE)
   expect_equal(get("outcome", pos = 1), TRUE)
   list.var <- ls(pos = 1)
   param <- get("pi_value_1_a",pos = 1)
   expect_equal("pi_value_1_a" %in% list.var, TRUE)
   expect_equal(param, get("pi_value_1", pos = 1))
})





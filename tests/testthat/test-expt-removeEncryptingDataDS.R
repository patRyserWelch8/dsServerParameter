
context("removeEncryptingDataDS::expt::no_settings")
test_that("no_setting",
{
  if(exists("settings",where = 1))
  {
    rm("settings",pos=1)
  }
  expect_equal(exists("settings",where = 1),FALSE)
  expect_error(removeEncryptingDataDS(),"SERVER::ERR::PARAM::001")
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

options(param.name.struct = "sharing")
options(param.sharing.allowed = 1) 

#("Step 0")
rm(list=ls(pos = 1),pos=1)

#("Step 0")
assign("pi_value_1", 100000, pos = 1)
assign("pi_value_2", 200000, pos = 1)
assign("pi_value_3", 300000, pos = 1)

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
assignCoordinatesDS(f$header,payload = f$payload,f$property.a,f$property.b,f$property.c,f$property.d)
assign("receiver.2.5", get("sharing", pos = 1), pos = 1)
rm(sharing,pos=1)
assign("sharing", get("master.3.5", pos = 1), pos=1)

encryptParamDS()

assign("master.4", get("sharing",pos = 1), pos = 1)


context("removeEncryptingDataDS::expt::")
test_that("computations",
{
  expect_equal(removeEncryptingDataDS(),TRUE)
  sharing <- get("sharing", pos = 1)

  expect_equal(length(sharing),11)
  expect_equal(all(c(settings$data,settings$no_columns, settings$no_rows, 
                     settings$index_x, settings$index_y, settings$param_names) %in% names(sharing)), TRUE)
})




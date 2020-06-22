source("definition_tests/def_sharing_structure.R")
source("definition_tests/def_getEncodedDataDS.R")
source("definition_tests/def_process.R")

context("encryptParamDS::expt::param")
test_that("parameters incorrect",
{
   expect_equal(encryptParamDS(),FALSE)
   expect_equal(encryptParamDS(1),FALSE)
   expect_equal(encryptParamDS(TRUE),FALSE)
   expect_equal(encryptParamDS(list()),FALSE)
})

context("encryptParamDS::expt::no_settings")
test_that("no_setting",
{
  if(exists("settings",where = 1))
  {
    rm("settings",pos=1)
  }
  expect_equal(exists("settings",where = 1),FALSE)
  expect_equal(encryptParamDS("pi_value"),FALSE)
})

#These steps are needed to complete the tests.....
pi_value = 1000
assign("pi_value",pi_value, pos=1)
rm(sharing,pos=1)

#"Step 0"
assignSharingSettingsDS()

context("encryptParamDS::expt::.is.param.valid")
test_that("is.param.valid",
{
  assign("pi_value",pi, pos=1)
  assign("pi_integer", as.integer(pi), pos=1)
  assign("char_value", "say hello to my little friend", pos=1)
  expect_equal(.is.param.valid(), FALSE)
  expect_equal(.is.param.valid(3.14), FALSE)
  expect_equal(.is.param.valid("inexistant"), FALSE)
  expect_equal(.is.param.valid("char_value"), FALSE)
  expect_equal(.is.param.valid(TRUE), FALSE)
  expect_equal(.is.param.valid("pi_value"), TRUE)
  expect_equal(.is.param.valid("pi_integer"), TRUE)
})

context("encryptParamDS::expt::get_shared_secrets")
test_that("no_sharing",
{
  expect_equal(exists("settings",where = 1),TRUE)
  expect_equal(exists("sharing",where = 1),FALSE)
  expect_equal(.get.shared.secrets(),list())
})

context("encryptParamDS::expt::.is.shared.secrets.valid")
test_that("no_sharing",
{
  expect_equal(exists("settings",where = 1),TRUE)
  expect_equal(exists("sharing",where = 1),FALSE)
  expect_equal(.is.shared.secrets.valid(pi_value),FALSE)
})

context("encryptParamDS::expt::get_shared_secrets")
test_that("setting has been created",
{
  expect_equal(exists("settings",where = 1),TRUE)
  expect_equal(exists("sharing",where = 1),FALSE)
  expect_equal(.get.shared.secrets(),list())
})

encryptDataDS(TRUE, FALSE)
master.1 <- get("sharing", pos=1)
master.encrypted   <- t(master.1$masking) %*% t(master.1$concealing)

#step 2
a <- getDataDS(master_mode =TRUE)
rm(sharing,pos=1)
assignDataDS(master_mode = FALSE,a$header,a$payload,a$property.a,a$property.b,a$property.c,a$property.d)
receiver.1 <- get("sharing",pos=1)

#step 3
expect_equal(encryptDataDS(FALSE, FALSE),TRUE)
receiver.2 <- get("sharing",pos=1)

#"step 4"
b <- getDataDS(master_mode =  FALSE)
rm(sharing,pos=1)
assign("sharing", master.1, pos=1)
assignDataDS(master_mode = TRUE, b$header,b$payload,b$property.a,b$property.b,b$property.c,b$property.d)
master.2 <- get("sharing",pos=1)

#"step 5"
decryptDataDS()
master.3 <- get("sharing",pos=1)

context("encryptParamDS::expt::get_shared_secrets")
test_that("sharing_exists",
{
  expect_equal(exists("settings",where = 1),TRUE)
  expect_equal(exists("sharing",where = 1),TRUE)
  expect_equal(is.list(.get.shared.secrets()),TRUE)
  expect_equal( c("decrypted") %in% names(sharing), TRUE)
})

context("encryptParamDS::expt::.is.shared.secrets.valid")
test_that("sharing and encryption exists",
{
  expect_equal(exists("settings",where = 1),TRUE)
  expect_equal(exists("sharing",where = 1),TRUE)
  expect_equal(.is.shared.secrets.valid(master.3),TRUE)
  expect_equal(.is.shared.secrets.valid(master.2),FALSE)
  expect_equal(.is.shared.secrets.valid(master.1),FALSE)
})

context("encryptParamDS::expt::..compute.encoding.ratio")
test_that("params",
{
  expect_error(.compute.encoding.ratio())
  expect_equal(.compute.encoding.ratio(NULL,"wrong_variable", 4,3),0)
  expect_equal(.compute.encoding.ratio(master.3$decrypted,"wrong_variable", 4,3),0)
  expect_equal(.compute.encoding.ratio(master.3$decrypted,"pi_value", 4,3)==0,FALSE)
  
})

#NEED to test encrypt parameter


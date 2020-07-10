context("assignParamSettingsDS::smk::incorrect_outcome")
test_that("everything is incorrect",
{
  if (exists("settings",where = 1))
  {
    rm("settings", pos=1)
  }
  expect_equal(exists("settings", where = 1), FALSE)
  expect_equal(assignParamSettingsDS(),FALSE)
  expect_equal(.has.correct.data(param_names = c("first_var")), FALSE)
  expect_equal(.has.correct.data(param_names = c("first_var", "second_var")),FALSE)
  expect_equal(.has.correct.data(param_names = c("first_var", "second_var","third_var")),FALSE)
  expect_equal(.has.correct.data(param_names = c("first_var",1)), FALSE)
  expect_equal(.has.correct.data(param_names = c(1,2,4)),FALSE)
  expect_equal(.has.correct.data(param_names = "hello"),FALSE)
  expect_equal(.has.correct.data(param_names = TRUE),FALSE)
  
  
})


context("assignParamSettingsDS::expt::.has.correct.data")
test_that("everyting is not correct",
{
    expect_equal(.has.correct.data(),FALSE)
    expect_equal(.has.correct.data(param_names = c(1)),FALSE)
    expect_equal(.has.correct.data(param_names = c("first_var")),FALSE)
    assign("first_var",1, pos=1)
    expect_equal(.has.correct.data(param_names = c("first_var", 1)),FALSE)
    expect_equal(.has.correct.data(param_names = c("first_var", "second_var")),FALSE)
    expect_equal(.has.correct.data(param_names = "hello"),FALSE)
    expect_equal(.has.correct.data(param_names = 1),FALSE) 
    expect_equal(.has.correct.data(param_names = list()),FALSE)
    
})

context("assignParamSettingsDS::expt::.init.coordinates.ratios")
test_that("everyting is incorrect",
{
  expect_equal(.init.coordinates.ratios(param_names = c("first_var"), NULL), list())
  expect_equal(.init.coordinates.ratios(param_names = c("first_var", "second_var"), 3),list())
  expect_equal(.init.coordinates.ratios(param_names = c("first_var", "second_var","third_var"), "HELLO"),list())
})



assignSharingSettingsDS()
assign("first_var",1, pos=1)
assign("second_var",1, pos=1)
assign("third_var",1, pos=1)


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


context("assignParamSettingsDS::smk::correct_outcome")
test_that("everything is correct",
{
  expect_equal(exists("settings", where = 1), TRUE)
  expect_equal(assignParamSettingsDS(c("first_var")),TRUE)
  expect_equal(assignParamSettingsDS(c("first_var", "second_var")),TRUE)
  expect_equal(assignParamSettingsDS(c("first_var", "second_var","third_var")),TRUE)
})
context("assignParamSettingsDS::expt::.has.correct.data")
test_that("everyting is correct",
{
  expect_equal(.has.correct.data(param_names = c("first_var")), TRUE)
  expect_equal(.has.correct.data(param_names = c("first_var", "second_var")),TRUE)
  expect_equal(.has.correct.data(param_names = c("first_var", "second_var","third_var")),TRUE)
})

context("assignParamSettingsDS::expt::.init.coordinates.ratios")
test_that("everyting is correct",
{
  sharing <- .init.coordinates.ratios(param_names = c("first_var"), get("sharing",pos = 1))
  expect_equal("index_x" %in% names(sharing), TRUE)
  expect_equal("index_y" %in% names(sharing), TRUE)
  expect_equal("param_names" %in% names(sharing), TRUE)
  expect_equal(length(sharing$index_x), 1)
  
  sharing <- .init.coordinates.ratios(param_names = c("first_var","second_var"), get("sharing",pos = 1))
  expect_equal("index_x" %in% names(sharing), TRUE)
  expect_equal("index_y" %in% names(sharing), TRUE)
  expect_equal("param_names" %in% names(sharing), TRUE)
  expect_equal(length(sharing$index_x), 2)
  
  sharing <- .init.coordinates.ratios(param_names = c("first_var","second_var","third_var"), get("sharing",pos = 1))
  expect_equal("index_x" %in% names(sharing), TRUE)
  expect_equal("index_y" %in% names(sharing), TRUE)
  expect_equal("param_names" %in% names(sharing), TRUE)
  expect_equal(length(sharing$index_x), 3)
  
  
})


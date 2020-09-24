source("definition_tests/def_getEncodedDataDS.R")
source('definition_tests/def_sendEncodedDataDS.R')

rm(list=ls(),pos=1)

options(param.name.struct = "sharing")
options(param.sharing.allowed = 0) 
assignSharingSettingsDS()

context("assignCoordinatesDS::expt:: incorrect parameters::no_coordinates::not_allowed")
test_that("parameters",
{
  expect_error(assignCoordinatesDS(), "SERVER::ERR::PARAM::001")
  expect_error(assignCoordinatesDS(1), "SERVER::ERR::PARAM::001")
  expect_error(assignCoordinatesDS("FM1",TRUE), "SERVER::ERR::PARAM::001")
  expect_error(assignCoordinatesDS("FM1","123,123","WRONG"), "SERVER::ERR::PARAM::001")
  expect_error(assignCoordinatesDS("FM1","123,123",1,"WRONG" ), "SERVER::ERR::PARAM::001")
  expect_error(assignCoordinatesDS("FM1","123,123",1,13,"WRING" ), "SERVER::ERR::PARAM::001")
  expect_error(assignCoordinatesDS("FM1","123,123",1,13,2.3,"INCORRECT" ), "SERVER::ERR::PARAM::001")
})



context("assignCoordinatesDS::expt:: correct parameters::no_coordinates::not_allowed")
test_that("parameters",
{
  
  expect_error(assignCoordinatesDS(header = "FM1", 
                                   payload = "0.5,0.5,0.5,0.5", 
                                   property.a = as.numeric(object.size("0.5,0.5,0.5, 0.5")),
                                   property.b = 2,
                                   property.c = as.numeric(Sys.time()) /567,
                                   property.d = 134893344), "SERVER::ERR::PARAM::001")
  
})

context("assignCoordinatesDS::expt::.save_coordinates::no_coordinates")
test_that("no_coordinates",
{
  expect_equal(exists("sharing",where = 1), FALSE)
  .save.coordinates()
  expect_equal(exists("sharing",where = 1), FALSE)
  
})


context("assignCoordinatesDS::expt::.create.data::no_coordinates")
test_that("no_coordinates",
{
   result <- .create.data()
   expect_equal(rep(0,4),result)
})

context("assignCoordinatesDS::expt::.is.assigned.coordinates.correct::no_coordinates")
test_that("no_coordinates",
{
  expect_equal(.is.assigned.coordinates.correct(),FALSE)
    
})

rm(list=ls(),pos=1)

options(param.name.struct = "sharing")
options(param.sharing.allowed = 1) 


#("Step 0")
assign("pi_value_1",100000, pos = 1)
assign("pi_value_2",200000, pos = 1)
assign("pi_value_3",300000, pos = 1)

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

assignParamSettingsDS(c("pi_value_1","pi_value_2","pi_value_3"))
master.3.5 <- get("sharing",pos=1)

f<- getCoordinatesDS()
rm(sharing,pos=1)
assign("sharing", receiver.2, pos=1)


context("assignCoordinatesDS::expt:: incorrect parameters::with_coordinates:: allowed")
test_that("parameters",
{
  expect_error(assignCoordinatesDS(), "SERVER::ERR::PARAM::006")
  expect_error(assignCoordinatesDS(1), "SERVER::ERR::PARAM::005")
  expect_error(assignCoordinatesDS("FM1",TRUE), "SERVER::ERR::PARAM::005")
  expect_error(assignCoordinatesDS("FM1","123,123","WRONG"), "SERVER::ERR::PARAM::005")
  expect_error(assignCoordinatesDS("FM1","123,123",1,"WRONG" ), "SERVER::ERR::PARAM::005")
  expect_error(assignCoordinatesDS("FM1","123,123",1,13,"WRING" ), "SERVER::ERR::PARAM::005")
  expect_error(assignCoordinatesDS("FM1","123,123",1,13,2.3,"INCORRECT" ), "SERVER::ERR::PARAM::005")
})

context("assignCoordinatesDS::expt:: correct parameters::with_coordinates")
test_that("parameters",
{
  
     expect_equal(assignCoordinatesDS(header = "FM1",
     payload = "0.5,0.5,0.5, 0.5",
     property.a = as.numeric(object.size("0.5,0.5,0.5, 0.5")),
     property.b = 2,
     property.c = as.numeric(Sys.time()) /567,
     property.d = 134893344), TRUE)
  
  
  
  
})

context("assignCoordinatesDS::expt::.save_coordinates::with_coordinates")
test_that("with_coordinates",
{
  expect_equal(exists("sharing",where = 1), TRUE)
  .save.coordinates(c(0.5, 0.5,0.5,0.5),2)
  expect_equal(exists("sharing",where = 1), TRUE)
  list.fields <- names(sharing)
  expect_equal(settings$index_x %in% list.fields, TRUE)
  expect_equal(settings$index_y %in% list.fields, TRUE)
  expect_equal(length(sharing[[settings$index_x]]),2)
  expect_equal(length(sharing[[settings$index_y]]),2)
})


context("assignCoordinatesDS::expt::.create.data::with_coordinates")
test_that("with_coordinates",
{
  result <- .create.data()
  expect_equal(rep(0,4),result)
  
  result <- .create.data(c(0.5,0.5,0.5,0.5),2)
  expect_equal(rep(0,4),result)
  
  result <- .create.data(c(0.5,0.5,0.5,0.5),"a")
  expect_equal(rep(0,4),result)
  
  result <- .create.data("a;b;c;d",2)
  expect_equal(rep(0,4),result)
  
  result <- .create.data("0.5;",1)
  expect_equal(rep(0,4),result)
  
  
  result <- .create.data("0.5;0.5;0.5;0.5",2)
  expect_equal(c(0.5,0.5,0.5,0.5),result)
})

context("assignCoordinatesDS::expt::.is.assigned.coordinates.correct::with_coordinates")
test_that("with_coordinates",
{
  expect_equal(.is.assigned.coordinates.correct(),TRUE)
})





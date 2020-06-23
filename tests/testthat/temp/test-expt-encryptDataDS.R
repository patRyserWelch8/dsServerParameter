source("definition_tests/def_sharing_structure.R")
source("definition_tests/def_process.R")

context("encryptDataDS::expt::incorrect_parameters")
test_that("variables exists",
{
  expect_equal(encryptDataDS(123,134), FALSE)
  expect_equal(encryptDataDS("123","134"), FALSE)
  expect_equal(encryptDataDS(TRUE,"134"), FALSE)
  expect_equal(encryptDataDS("123",FALSE), FALSE)
})

context("encryptDataDS::expt::no_settings")
{
  if (exists("settings",where = 1))
  {
    rm("settings", pos=1)
  }
  expect_equal(encryptDataDS(123,134), FALSE)
  expect_equal(encryptDataDS("123","134"), FALSE)
  expect_equal(encryptDataDS(TRUE,"134"), FALSE)
  expect_equal(encryptDataDS("123",FALSE), FALSE)
  expect_equal(encryptDataDS(123,134), FALSE)
  expect_equal(encryptDataDS("123","134"), FALSE)
  expect_equal(encryptDataDS(TRUE,"134"), FALSE)
  expect_equal(encryptDataDS(FALSE,FALSE), FALSE)
}

assignSharingSettingsDS()

context("encryptDataDS::expt::.define_no_rows")
test_that("define.no.rows",
{
  #numeric and odd number
  expect_equal(is.integer(.define_no_rows()),TRUE)
  expect_equal(.define_no_rows() %% 2 == 1, TRUE)
  expect_equal(.define_no_rows() %% 2 == 0, FALSE)
  
  #correct range
  no.rows <- .define_no_rows()
  expect_equal((no.rows >= 11 & no.rows <= 21),TRUE)
  
})
assignSharingSettingsDS()
settings <- get("settings", pos = 1)

context("encryptDataDS::expt::.define_no_columns")
test_that("numeric and odd number",
{
  
  expect_equal(is.integer(.define_no_columns()),TRUE)
  expect_equal(.define_no_columns() %% 2 == 1, TRUE)
})

test_that("correct range",
{
 
  no.columns <- .define_no_columns()
  expect_equal((no.columns >= 13 & no.columns <= 23),TRUE)
})

test_that("correct range",
{
  no.rows = 15
  #numeric and odd number
  expect_equal(is.integer(.define_no_columns(no.rows = no.rows)),TRUE)
  expect_equal(.define_no_columns(no.rows = no.rows) %% 2 == 1, TRUE)
})

test_that("incorrect input",
{
  no.rows = "a"
  expect_error(.define_no_columns(no.rows = no.rows))
})

context("encryptDataDS::expt::.createMatrixRUnif")
test_that("no argument",
{
  createdMatrix <- .createMatrixRUnif()
  expect_equal(nrow(createdMatrix) == settings$min_rows, TRUE)
  expect_equal(ncol(createdMatrix) == settings$min_columns, TRUE)
  expect_equal(all(createdMatrix <= 1, TRUE),TRUE)
})


test_that("no row",
{
  createdMatrix <- .createMatrixRUnif(no.rows = 10)
  expect_equal(nrow(createdMatrix) == 11, TRUE)
  expect_equal(ncol(createdMatrix) == 13, TRUE)
  expect_equal(all(createdMatrix <= 1, TRUE),TRUE)
}) 

test_that("no row correct",
{
 
  createdMatrix <- .createMatrixRUnif(no.rows = 12)
  expect_equal(nrow(createdMatrix) == 12, TRUE)
  expect_equal(ncol(createdMatrix) == 13, TRUE)
  expect_equal(all(createdMatrix <= 1, TRUE),TRUE)
})

test_that("no column incorrect",
{
  createdMatrix <- .createMatrixRUnif(no.rows = 13, no.columns =11)
  expect_equal(nrow(createdMatrix) == 11, TRUE)
  expect_equal(ncol(createdMatrix) == 13, TRUE)
  expect_equal(all(createdMatrix <= 1, TRUE),TRUE)
})

test_that("no row  and columns correct",
{ 
  createdMatrix <- .createMatrixRUnif(no.rows = 15, no.columns = 17)
  expect_equal(nrow(createdMatrix) == 15, TRUE)
  expect_equal(ncol(createdMatrix) == 17, TRUE)
  expect_equal(all(createdMatrix <= 1, TRUE),TRUE)
})


test_that("no row  and columns, min value correct",
{
  expect_warning(.createMatrixRUnif(no.rows = 15, no.columns = 17, min.value = 15))

}) 

test_that("no row  and columns, min value incorrect", 
{  

  createdMatrix <- .createMatrixRUnif(no.rows = settings$min_rows+1, no.columns = settings$min_columns+1, min.value = -12)
  expect_equal(nrow(createdMatrix) == settings$min_rows+1, TRUE)
  expect_equal(ncol(createdMatrix) == settings$min_columns+1, TRUE)
  expect_equal(all(createdMatrix >= -12 & createdMatrix <= 1, TRUE),TRUE)
})
 
test_that("no row  and columns, min value, max value correct",
{
  createdMatrix <- .createMatrixRUnif(no.rows = 15, no.columns = 17, min.value = -12, max.value = 298)
  expect_equal(nrow(createdMatrix) == 15, TRUE)
  expect_equal(ncol(createdMatrix) == 17, TRUE)
  expect_equal(all(createdMatrix >= -12 & createdMatrix <= 298, TRUE),TRUE)
})

context("encryptDataDS::expt::.is.encrypted.valid")
test_that(".is.encrypted.valid",
{
  #correct structure
  encryptDataDS(TRUE,TRUE)
  sharing <- get("sharing",pos = 1)
  expect_equal(.is.encrypted.valid(sharing,expected.list),TRUE)

  correct.structure  <- list(data = c(1:4),
  concealing.matrix = matrix(1:2,1,1),
  masking.matrix = matrix(1:2,1,1),
  encrypted.matrix = matrix(1:2,1,1),
  index = 3)
  assign("sharing",correct.structure, pos=1)
  expect_equal(.is.encrypted.valid(sharing,expected.list),TRUE)

  #incorrect structure
  incorrect.structure <- list()
  assign("sharing",incorrect.structure, pos=1)
  sharing <- get("sharing",pos = 1)
  expect_equal(.is.encrypted.valid(sharing,expected.list),FALSE)

  incorrect.structure <- list(master.vector = c(1,2,3),
  concealing.matrix = matrix(1:2,1,1))
  assign("sharing",incorrect.structure, pos=1)
  sharing <- get("sharing",pos = 1)
  expect_equal(.is.encrypted.valid(sharing,expected.list),FALSE)
})

context("encryptDataDS::expt::.create.structure.master")
test_that(".create.structure.master",
{
  expected.list <- c("concealing","masking","no_columns","no_rows")
  
  sharing <- .create.structure.master(min=1, max=2,no.rows=11, no.columns=13)
  expect_equal(is.list(sharing),TRUE)
  expect_equal(all(expected.list %in% names(sharing), TRUE), TRUE)
  expect_equal(length(sharing) == length(expected.list), TRUE)

  expect_equal(is.matrix(sharing$masking), TRUE)
  expect_equal(is.matrix(sharing$concealing), TRUE)
})



context("encryptDataDS::expt::.create.structure.receiver")
test_that("received matrix does not exist",
{
  expected.list <- c("concealing.matrix","masking.matrix","received.matrix")
  #the received matrix does not exists
  sharing <- .create.structure.receiver(4,23)
  expect_equal(is.list(sharing),TRUE)
  expect_equal(all(expected.list %in% names(sharing), FALSE), FALSE)
  expect_equal(length(sharing) == 0, TRUE)
  
  expect_equal(is.vector(sharing$master.vector), FALSE)
  expect_equal(is.matrix(sharing$encoded.matrix), FALSE)
  expect_equal(is.matrix(sharing$masking.matrix), FALSE)
  expect_equal(is.matrix(sharing$concealing.matrix), FALSE)
  
  a.list <- list(element = 3.1427)
  assign("sharing",a.list, pos=1)
  sharing <- .create.structure.receiver(4,23)
  expect_equal(is.list(sharing),TRUE)
  expect_equal(all(expected.list %in% names(sharing), FALSE), FALSE)
  expect_equal(length(sharing) == 0, TRUE)
  
  expect_equal(is.vector(sharing$master.vector), FALSE)
  expect_equal(is.matrix(sharing$encoded.matrix), FALSE)
  expect_equal(is.matrix(sharing$masking.matrix), FALSE)
  expect_equal(is.matrix(sharing$concealing.matrix), FALSE)
})

#start again from clean environment to test the outcome
rm(list=ls(),pos=1)
#("Step 0")
pi_value = 1000
assignSharingSettingsDS()

#("Step 1")

outcome <- encryptDataDS(master_mode = TRUE, preserve_mode = FALSE)
master.1 <- get("sharing",pos=1)
master.encrypted   <- t(master.1$masking) %*% t(master.1$concealing)

context("encryptDataDS::expt::correct outcome")
test_that("step 1",
{
  expect_equal(outcome,TRUE)
  expect_equal(exists("sharing",where=1),TRUE)
  sharing     <- get("sharing", pos = 1)
  expect_equal(all.equal(master.encrypted, sharing$encrypted),TRUE)
})


#("Step 2")
a <- getDataDS(master_mode =TRUE)
rm(sharing,pos=1)
assignDataDS(master_mode = FALSE,a$header,a$payload,a$property.a,a$property.b,a$property.c,a$property.d)
receiver.1 <- get("sharing",pos=1)

#("Step 3")
outcome <- encryptDataDS(FALSE, FALSE)
receiver.2 <- get("sharing",pos=1)

print(dim(master.encrypted))
print(dim(receiver.1$received))

test_that("step 3",
{
  expect_equal(outcome, TRUE)
  print(names(receiver.2))
  expect_equal(all.equal(master.encrypted,receiver.1$received),TRUE)
  expect_equal(all.equal(receiver.1$received,receiver.2$masking),TRUE)
  receiver.encrypted <- master.encrypted %*% receiver.2$concealing
  expect_equal(all.equal(receiver.encrypted, receiver.2$encrypted),TRUE)
})

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
removeEncryptingDataDS()
receiver.3 <- get("sharing",pos=1)
outcome <-encryptDataDS(TRUE, TRUE)
receiver.4 <- get("sharing",pos=1)

test_that("step 6",
{
   expect_equal(outcome, TRUE)
   
})

#("step 7")
c <- getDataDS(master_mode = TRUE)
rm(sharing,pos=1)
assign("sharing", master.5, pos=1)
assignDataDS(master_mode = FALSE,c$header,c$payload,c$property.a,c$property.b,c$property.c,c$property.d)
master.6 <- get("sharing",pos=1)

#("step 8 ")
outcome <- encryptDataDS(FALSE, TRUE)
master.7 <- get("sharing",pos=1)

test_that("step 8",
{
  expect_equal(outcome, TRUE)
})





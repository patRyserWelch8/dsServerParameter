source("definition_tests/def_sharing_structure.R")
source("definition_tests/def_getEncodedDataDS.R")
source("definition_tests/def_process.R")


context("encodedParamDS::expt::param")
test_that("parameters incorrect",
{
   expect_equal(encodeParamDS(),FALSE)
   expect_equal(encodeParamDS(1),FALSE)
   expect_equal(encodeParamDS(TRUE),FALSE)
   expect_equal(encodeParamDS(list()),FALSE)
})

create.init.matrices.master()
transfer.matrices.from.master.to.receiver()
create.init.matrices.receiver()
transfer.matrices.from.receiver.to.master()

assign("pi_value",pi, pos=1)

test_that("parameters correct",
{
  
  expect_equal(encodeParamDS("pi_value"),TRUE)
  expect_equal(encodeParamDS("inexistant"),FALSE)
  
})

context("encodedParamDS::expt::.is.param.valid")
test_that("is.param.valid",
{
  assign("pi_value",pi, pos=1)
  assign("pi_integer", as.integer(pi), pos=1)
  assign("char_value", "say hello to my little friend", pos=1)
  expect_equal(.is.param.valid(), FALSE)
  expect_equal(.is.param.valid(3.14), FALSE)
  expect_equal(.is.param.valid("inexistant"), FALSE)
  expect_equal(.is.param.valid(TRUE), FALSE)
  expect_equal(.is.param.valid("pi_value"), TRUE)
  expect_equal(.is.param.valid("pi_integer"), TRUE)
})

context("encodeParamDS::expt::..is.sharing.valid")
test_that(".is.sharing.valid",
{
  create.init.matrices.master()
  transfer.matrices.from.master.to.receiver()
  create.init.matrices.receiver()
  transfer.matrices.from.receiver.to.master()
  #correct structure
  expect_equal(.is.sharing.valid(),TRUE)

  #incorrect structure
  incorrect.structure <- list()
  assign("sharing",incorrect.structure, pos=1)
  expect_equal(.is.structure.valid(),FALSE)
  incorrect.structure <- list(master.vector = c(1,2,3),
  concealing.matrix = matrix(1:2,1,1))
  assign("sharing",incorrect.structure, pos=1)
  expect_equal(.is.structure.valid(),FALSE)
})

context("encodeParamDS::expt::.decode.received.matrix")
test_that(".decode.received.matrix",
{  
  create.init.matrices.master()
  transfer.matrices.from.master.to.receiver()
  create.init.matrices.receiver()
  transfer.matrices.from.receiver.to.master()
  expect_equal(.decode.received.matrix(), NULL)  
  expect_equal(.decode.received.matrix("no_matrix", "no_matrix"), NULL) 
  expect_equal(.decode.received.matrix(sharing$masking, "no_matrix"), NULL) 
  #matrices have not the correct dimensions.
  result <- .decode.received.matrix(sharing$masking,matrix(c(1,3,3,4),2,2))
  expect_equal(is.matrix(result), TRUE) 
  expect_equal(all(result == 0), TRUE)
  
  #matrices have the correct dimensions.
  result <- .decode.received.matrix(sharing$masking,sharing$received.matrix)
  
  expect_equal(is.matrix(result), TRUE) 
  expect_equal(all(result == 0), FALSE)
})

context("encodeParamDS::expt::.compute.encoding.ratio")
test_that(".compute.encoding.ratio",
{
  create.init.matrices.master()
  transfer.matrices.from.master.to.receiver()
  create.init.matrices.receiver()
  transfer.matrices.from.receiver.to.master()
  
  rm("pi_value", pos = 1)
  decoded.matrix  <- .decode.received.matrix(sharing$masking.matrix, sharing$received.matrix)
  encoding.ratio  <- .compute.encoding.ratio(decoded.matrix,pi)
  expect_equal(encoding.ratio == 0, TRUE)
  encoding.ratio  <- .compute.encoding.ratio(decoded.matrix,"pi_value")
  expect_equal(encoding.ratio == 0, TRUE)
  encoding.ratio  <- .compute.encoding.ratio(NULL,"pi_value")
  expect_equal(encoding.ratio == 0, TRUE)
  encoding.ratio  <- .compute.encoding.ratio(c(1:4),"pi_value")
  expect_equal(encoding.ratio == 0, TRUE)
  assign("pi_value", pi, pos=1)
  encoding.ratio  <- .compute.encoding.ratio(matrix(c(1:4),2,2),"pi_value")
  expect_equal(encoding.ratio == 0, TRUE)
  encoding.ratio  <- .compute.encoding.ratio(decoded.matrix,"pi_value")
  expect_equal(encoding.ratio != 0, TRUE)
})

context("encodeParamDS::expt::.encode.parameter")
test_that(".encode.parameter",
{
  expect_equal(.encode.parameter(NULL,NULL) == 0, TRUE)
  expect_equal(.encode.parameter("NOT A VECTOR","NOT A NUMBER") == 0, TRUE)
  encoded.vector <-  expect_equal(.encode.parameter(sharing$master.vector, TRUE) == 0, TRUE)
  expect_equal(.encode.parameter("NOT A VECTOR","NOT A NUMBER") == 0, TRUE)
  
  #correct parameters
  assign("pi_value", pi, pos=1)
  vector            <- c(1:10)
  encoding.ratio    <- 0.1
  expected.vector   <- encoding.ratio * vector
  encoded.parameter <- .encode.parameter(vector, encoding.ratio)
  reverse     <- (1/encoding.ratio) * encoded.parameter 
  expect_equal(is.vector(encoded.parameter), TRUE)
  expect_equal(identical(encoded.parameter,expected.vector), TRUE)
  expect_equal(all.equal(reverse,vector), TRUE)
  
  
  
})


rm("sharing", pos = 1)

context("encodedParamDS::expt::no_sharing")
test_that("parameters incorrect",
{
  expect_equal(encodeParamDS(),FALSE)
  expect_equal(encodeParamDS(1),FALSE)
  expect_equal(encodeParamDS(TRUE),FALSE)
  expect_equal(encodeParamDS(list()),FALSE)
})

test_that("parameters correct",
{
  assign("pi_value",pi, pos=1)
  expect_equal(encodeParamDS("pi_value"),FALSE)
  expect_equal(encodeParamDS("inexistant"),FALSE)
  
})

context("encodedParamDS::expt::.is.param.valid")
test_that("is.param.valid",
{
  assign("pi_value",pi, pos=1)
  assign("pi_integer", as.integer(pi), pos=1)
  assign("char_value", "say hello to my little friend", pos=1)
  expect_equal(.is.param.valid(), FALSE)
  expect_equal(.is.param.valid(3.14), FALSE)
  expect_equal(.is.param.valid("inexistant"), FALSE)
  expect_equal(.is.param.valid(TRUE), FALSE)
  expect_equal(.is.param.valid("pi_value"), TRUE)
  expect_equal(.is.param.valid("pi_integer"), TRUE)
})

context("encodeParamDS::expt::..is.sharing.valid")
test_that(".is.sharing.valid no sharing",
{
  #correct structure
  expect_equal(.is.sharing.valid(),FALSE)
})


context("encodeParamDS::expt::.compute.encoding.ratio")
test_that(".compute.encoding.ratio",
{
  rm("pi_value", pos = 1)
  decoded.matrix  <-  NULL
  encoding.ratio  <- .compute.encoding.ratio(decoded.matrix,pi)
  expect_equal(encoding.ratio == 0, TRUE)
  encoding.ratio  <- .compute.encoding.ratio(decoded.matrix,"pi_value")
  expect_equal(encoding.ratio == 0, TRUE)
  encoding.ratio  <- .compute.encoding.ratio(NULL,"pi_value")
  expect_equal(encoding.ratio == 0, TRUE)
  encoding.ratio  <- .compute.encoding.ratio(c(1:4),"pi_value")
  expect_equal(encoding.ratio == 0, TRUE)
  assign("pi_value", pi, pos=1)
  encoding.ratio  <- .compute.encoding.ratio(matrix(c(1:4),2,2),"pi_value")
  expect_equal(encoding.ratio == 0, TRUE)
  encoding.ratio  <- .compute.encoding.ratio(decoded.matrix,"pi_value")
  expect_equal(encoding.ratio == 0, TRUE)
})
        

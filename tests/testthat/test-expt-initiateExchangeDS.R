source("definition_tests/def_sharing_structure.R")

context("initiateExchangeDS::expt::correct_parameters")
test_that("variables exists",
{
  rm(list = "sharing", pos=1)
  .test_sharing_is_created()
})


context("initiateExchangeDS::expt::.define_no_rows")
test_that("define.no.rows",
{
  #numeric and odd number
  expect_equal(is.integer(.define_no_rows()),TRUE)
  expect_equal(.define_no_rows() %% 2 == 1, TRUE)

  #correct range
  no.rows <- .define_no_rows()
  expect_equal((no.rows >= 11 & no.rows <= 21),TRUE)

})

context("initiateExchangeDS::expt::.define_no_columns")
test_that("define.no.columns",
{
  #numeric and odd number
  expect_equal(is.integer(.define_no_columns()),TRUE)
  expect_equal(.define_no_columns() %% 2 == 1, TRUE)

  #correct range
  no.columns <- .define_no_columns()
  expect_equal((no.columns >= 13 & no.columns <= 23),TRUE)

  no.rows = 15
  #numeric and odd number
  expect_equal(is.integer(.define_no_columns(no.rows = no.rows)),TRUE)
  expect_equal(.define_no_columns(no.rows = no.rows) %% 2 == 1, TRUE)

  #correct range
  no.columns <- .define_no_columns(no.rows = no.rows)
  expect_equal((no.columns >= 13 & no.columns <= 23),TRUE)

  no.rows = "a"
  #numeric and odd number
  expect_equal(.define_no_columns(no.rows = no.rows),0)
})


context("initiateExchangeDS::expt::.createMatrixRUnif")
test_that(".createMatrixRUnif",
{
    #no argument
    createdMatrix <- .createMatrixRUnif()
    expect_equal(nrow(createdMatrix) == 11, TRUE)
    expect_equal(ncol(createdMatrix) == 13, TRUE)
    expect_equal(all(createdMatrix < 1, TRUE),TRUE)

    #no row
    createdMatrix <- .createMatrixRUnif(no.rows = 10)
    expect_equal(nrow(createdMatrix) == 11, TRUE)
    expect_equal(ncol(createdMatrix) == 13, TRUE)
    expect_equal(all(createdMatrix < 1, TRUE),TRUE)

    #no row correct
    createdMatrix <- .createMatrixRUnif(no.rows = 12)
    expect_equal(nrow(createdMatrix) == 12, TRUE)
    expect_equal(ncol(createdMatrix) == 13, TRUE)
    expect_equal(all(createdMatrix < 1, TRUE),TRUE)

    #no column incorrect
    createdMatrix <- .createMatrixRUnif(no.rows = 13, no.columns =11)
    expect_equal(nrow(createdMatrix) == 11, TRUE)
    expect_equal(ncol(createdMatrix) == 13, TRUE)
    expect_equal(all(createdMatrix < 1, TRUE),TRUE)

    #no row  and columns correct
    createdMatrix <- .createMatrixRUnif(no.rows = 15, no.columns = 17)
    expect_equal(nrow(createdMatrix) == 15, TRUE)
    expect_equal(ncol(createdMatrix) == 17, TRUE)
    expect_equal(all(createdMatrix < 1, TRUE),TRUE)

    #no row  and columns, min value correct
    #createdMatrix <- .createMatrixRUnif(no.rows = 15, no.columns = 17, min.value = 12)
   # expect_equal(nrow(createdMatrix) == 15, TRUE)
    #expect_equal(ncol(createdMatrix) == 17, TRUE)
    #expect_equal(all(is.nan(createdMatrix), TRUE),TRUE)


    #no row  and columns, min value incorrect
    createdMatrix <- .createMatrixRUnif(no.rows = 15, no.columns = 17, min.value = -12)
    expect_equal(nrow(createdMatrix) == 15, TRUE)
    expect_equal(ncol(createdMatrix) == 17, TRUE)
    expect_equal(all(createdMatrix >= -12 & createdMatrix <= 1, TRUE),TRUE)

    #no row  and columns, min value, max value correct
    createdMatrix <- .createMatrixRUnif(no.rows = 15, no.columns = 17, min.value = -12, max.value = 298)
    expect_equal(nrow(createdMatrix) == 15, TRUE)
    expect_equal(ncol(createdMatrix) == 17, TRUE)
    expect_equal(all(createdMatrix >= -12 & createdMatrix <= 298, TRUE),TRUE)
})

context("initiateExchangeDS::expt::.occult")
test_that(".occult",
{
    #incorrect parameters
    a.vector <- rep(2,4)
    a.matrix <- matrix(a.vector,2,2)
    occultMatrix <- .occult()
   
    expect_equal(is.null(.occult()), TRUE)
    expect_equal(is.null(.occult(a.matrix)), TRUE)
    expect_equal(is.null(.occult(a.matrix,a.matrix)), TRUE)
    expect_equal(is.null(.occult(a.matrix,NULL,a.vector)), TRUE)
    expect_equal(is.null(.occult("a",1,FALSE)), TRUE)
    expect_equal(is.null(.occult(a.matrix,a.matrix,a.vector)), FALSE)


    #incorrect length - vector and nrow of concealing matrix
    a.vector    <- rep(2,6)
    result      <- .occult(a.matrix,a.matrix,a.vector)
    expect_equal(all(result == 0,TRUE),TRUE)
    expect_equal(nrow(result) == nrow(a.matrix),TRUE)
    expect_equal(ncol(result) == ncol(a.matrix),TRUE)

    #correct paramaters
    a.vector          <- c(10,20,30)
    concealing.matrix <- matrix(c(1:15),3,5)
    masking.matrix    <- matrix(c(101:125),5,5)
    
    column.hidden     <- concealing.matrix
    column.hidden[,3] <- a.vector
    masking.t         <- t(masking.matrix)
    hidden.t          <- t(column.hidden)
    expected.results  <- masking.t %*% hidden.t
  
    a.vector          <- c(10,20,30)
    concealing.matrix <- matrix(c(1:15),3,5)
    masking.matrix    <- matrix(c(101:125),5,5)
    results           <- .occult(masking.matrix = masking.matrix,
                                 concealing.matrix = concealing.matrix, 
                                 concealed.vector = a.vector)
    print(results)
    
    #expected.result
   
    expect_equal(all(expected.results == results,TRUE),TRUE)
    expect_equal(nrow(results) == nrow(masking.t), TRUE)
    expect_equal(ncol(results) == ncol(hidden.t), TRUE)
})


context("initiateExchangeDS::expt::.is.structure.valid")
test_that(".is.structure.valid",
{
  #correct structure
  initiateExchangeDS()
  expect_equal(.is.structure.valid(),TRUE)

  correct.structure  <- list(master.vector = c(1:4),
                               concealing.matrix = matrix(1:2,1,1),
                               masking.matrix = matrix(1:2,1,1),
                               encoded.matrix = matrix(1:2,1,1))
  assign("sharing",correct.structure, pos=1)
  expect_equal(.is.structure.valid(),TRUE)

  #incorrect structure
  incorrect.structure <- list()
  assign("sharing",incorrect.structure, pos=1)
  expect_equal(.is.structure.valid(),FALSE)

  incorrect.structure <- list(master.vector = c(1,2,3),
                              concealing.matrix = matrix(1:2,1,1))
  assign("sharing",incorrect.structure, pos=1)
  expect_equal(.is.structure.valid(),FALSE)
})

.test.create.matrix.parameters.correct <- function(data, no.columns)
{
  result          <- .create.matrix(data, no.columns)
  expected.matrix <- .find.expected.results(data, no.columns)
  
  expect_equal(nrow(result),nrow(expected.matrix))
  expect_equal(ncol(result),ncol(expected.matrix))
  expect_equal(as.logical(all(result == expected.matrix)), TRUE)
}


.test.create.matrix.parameters.incorrect <- function()
{
  empty.matrix   <- matrix(c(0,0,0,0),2,2)
  result         <- .create.matrix()
  expect_equal(as.logical(all(result == empty.matrix)), TRUE)
  
  data = "123,123"
  no.columns = 2
  expected.matrix <- .find.expected.results(data, no.columns)
 
  result <- .create.matrix(data,no.columns)
  
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result),2)
  expect_equal(all(result == empty.matrix), TRUE)
}

.test.save.matrix.parameters.correct <- function()
{
  empty.matrix   <- matrix(c(0,0), 2,2)
  .save(empty.matrix)
  list.variables <- ls(pos=1)
  sharing <- get("sharing", pos=1)
  expect_equal("sharing" %in% list.variables, TRUE)
  expect_equal("received" %in% names(sharing), TRUE)
  
  expect_equal(is.matrix(sharing$received),TRUE)
}

.test.save.matrix.parameters.incorrect <- function()
{
  rm("sharing",pos=1)
  .save()
  list.variables <- ls(pos=1)
  expect_equal("sharing" %in% list.variables, FALSE)
  
 
}
.find.expected.results <- function(data, no.columns)
{
  data.list       <- strsplit(data,";")
  data.vector     <- unlist(data.list)
  no.rows         <- length(data.vector)/no.columns
  data.numeric    <- as.numeric(data.vector)
  expected.matrix <- matrix(data=data.numeric,nrow=no.rows, ncol= no.columns)
}

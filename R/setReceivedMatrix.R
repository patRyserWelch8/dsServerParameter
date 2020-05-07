#'@name setReceivedMatrixDS
#'@title  decodes some data and transform them into a matrix
#'@description This server function assigns some values into a specific structure.
#'@export

#'@export

setReceivedMatrixDS <- function(data = "", no.column = 0)
{
  outcome <- FALSE
  print("A")
  if (is.character(data) & is.numeric(no.column))
  {
    print("B")
    received.matrix <- .create.matrix(data,no.column)
    print("C")
    .save.matrix(received.matrix)
    print("D")
    outcome <- .is.matrix.created()
  }
  return(outcome)
}

.save.matrix <- function(received.matrix = NULL)
{
  if(!is.null(received.matrix))
  {
    if (is.matrix(received.matrix))
    {
      if (!exists("sharing",globalenv()))
      {
        sharing <- list()
      }

      sharing$received.matrix <- received.matrix
      print(sharing)
      assign("sharing", sharing, pos = 1)
    }
  }
}

.create.matrix <- function(data = "",  no.column = 0)
{

    data.list    <- strsplit(data,",")
    data.vector  <- unlist(data.list)
    data.numeric <- as.numeric(data.vector)
    no.row <- length(data.numeric)/no.column
    received.matrix <- matrix (rep(0),2,2)

    if (no.row > 1 & no.column > 1)
    {
      received.matrix <- matrix(data=data.numeric,nrow=no.row, ncol= no.column)
    }
    return(received.matrix)
}


.is.matrix.created <- function()
{
  outcome <- FALSE

  if (exists("sharing",globalenv()))
  {
    received.matrix.exists <- ("received.matrix" %in% names(sharing))
    if (received.matrix.exists)
    {
      outcome <- is.matrix(sharing$received.matrix)
      print(outcome)
    }

  if (exists("received.matrix",globalenv()))
  {
    retrieved.matrix <- get("received.matrix",envir=globalenv())
    outcome <- is.matrix(retrieved.matrix)
  }
  return(outcome)
  }
}


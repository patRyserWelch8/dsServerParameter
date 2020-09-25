#obsolete ....
.get_received_data <- function()
{
  outcome <- list()
  
  if(exists(settings$name.struct, where =1))
  {
    outcome <- get(settings$name.struct, pos=1) 
  }
  
  return(outcome)
}

.is.received.data.valid <- function(received.data)
{
  correct       <- FALSE
  expected.list <- c(settings$received,settings$masking)
  if (is.list(received.data))
  {
      attributes.exist <- names(received.data) %in% expected.list
      total.correct    <- sum(attributes.exist == TRUE)
      correct          <- (total.correct == length(expected.list))
      if (correct)
      {
         correct       <- correct & 
                          is.matrix(received.data[[settings$masking]]) & 
                          is.matrix(received.data[[settings$received]])
      }
  }
  return(correct)
}

.decrypt.received.matrix <- function(masking.matrix = NULL, received.matrix = NULL)
{
  result <- NULL
  
  if(is.matrix(masking.matrix) & is.matrix(received.matrix))
  {
    masking.inverse  <- solve(t(masking.matrix))
    no.col           <- ncol(masking.inverse)
    no.row           <- nrow(received.matrix)
    result           <- matrix(rep(0,no.row * no.col),no.row, no.col)
   
    if (no.row == no.col)
    {
      result <- masking.inverse %*% received.matrix
    }
  }
  
  return(result)
}

.is.decrypted.data.valid <- function(expected.list)
{
  correct <- FALSE
  sharing       <- get(settings$name.struct,pos=1)
      
  if (is.list(sharing))
  {
        attributes.exist <- names(sharing) %in% expected.list
        total.correct    <- sum(attributes.exist == TRUE)
        correct          <- (total.correct == length(expected.list))
   }
  
  return(correct)
}

#'@name decryptDataDS
#'@title  decrypt data received from another server
#'@description This server function decrypts some received data from a server acting as a "receiver"
#'@export
decryptDataDS   <- function()
{
  outcome       <- FALSE
  expected.list <- c(settings$received,settings$masking, settings$decrypted)
  
  if (is.sharing.allowed())
  {
    sharing <- .get_received_data()
    if(.is.received.data.valid(sharing))
    {
      sharing[[settings$decrypted]] <- .decrypt.received.matrix(sharing[[settings$masking]], 
                                                                sharing[[settings$received]])
      assign("sharing", sharing, pos = 1)
      outcome                       <- .is.decrypted.data.valid(expected.list)
    }
    else
    {
      stop("SERVER::ERR::PARAM::007")
    }
  }
  else
  {
    stop("SERVER::ERR::PARAM::001")
  }
  return(outcome)
}

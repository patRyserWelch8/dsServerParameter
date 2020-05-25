
.get_encrypted_data <- function()
{
  outcome <- list()
  
  if(exists(settings$name.struct, where =1))
  {
    outcome <- get(settings$name.struct, pos=1) 
    
  }
  return(outcome)
}

.is.encrypted.data.valid <- function(encrypted.data)
{
  correct <- FALSE
  expected.list <- c(settings$encrypted,settings$masking)
  
  if (is.list(encrypted.data))
  {
    list.attributes <- names(encrypted.data)
    attributes.exist <- list.attributes %in% expected.list
    total.correct = sum(attributes.exist == TRUE)
    correct <- (total.correct == length(expected.list))
  }
  return(correct)
}

.decrypt.received.matrix <- function(masking.matrix = NULL, encrypted.matrix = NULL)
{
  result <- NULL
  if(is.matrix(masking.matrix) & is.matrix(encrypted.matrix))
  {
    masking.inverse  <- solve(masking.matrix)
    no.col           <- ncol(masking.inverse)
    no.row           <- nrow(encrypted.matrix)
    result           <- matrix(rep(0,no.row * no.col),no.row, no.col)
    
    if (no.row == no.col)
    {
      result <- masking.inverse %*% encrypted.matrix
    }
  }
  
  return(result)
}

.is.decrypted.data.valid <- function(expected.list)
{
  correct <- FALSE
  
  if(exists("sharing",where=1))
  {
    
    
    sharing       <- get("sharing",pos=1)
    
    if (is.list(sharing))
    {
      list.attributes  <- names(sharing)
      attributes.exist <- list.attributes %in% expected.list
      total.correct    <- sum(attributes.exist == TRUE)
      correct          <- (total.correct == length(expected.list))
    }
  }
  return(correct)
}



#'@name decryptDataDS
#'@title  decrypt data received from another server
#'@description This server function decrypts some received data from a server acting as a "receiver"
#'@export
decryptDataDS  <- function()
{
  outcome <- FALSE
  sharing <- .get_encrypted_data()
  if(.is.encrypted.data.valid(sharing))
  {
    sharing[[settings$decrypted]] = .decrypt.received.matrix(sharing[[settings$masking]], sharing[[settings$encrypted]])
    assign("sharing", sharing, pos=1)
    expected.list <- c(settings$encrypted,settings$masking, settings$decrypted)
    outcome <- .is.decrypted.data.valid(expected.list)
  }
  return(outcome)
}

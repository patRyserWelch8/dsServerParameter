.is.sharing.valid <- function()
{
  correct <- FALSE
  expected.list <- c("concealing.matrix","encoded.matrix","masking.matrix","master.vector","received.matrix")
  if(exists("sharing",where = 1))
  {
    
    if (is.list(sharing))
    {
      no.elements <- length(sharing)
      if (no.elements == length(expected.list))
      {
        list.attributes <- ls(sharing)
        attributes.exist <- list.attributes == expected.list
        correct <- sum(attributes.exist) == no.elements
      }
    }
  }
  return(correct)
}

.is.param.valid <- function(param.name = NULL)
{
  #use existsDS...
  outcome <- FALSE
  if(is.character(param.name))
  {
    if(exists(param.name,where=1))
    {
      param <- get(param.name, pos=1)
      if(is.numeric(param))
      {
        outcome <- TRUE
      }
    }
  }
  return(outcome)
}


.decode.received.matrix <- function(masking.matrix = NULL, encoded.matrix = NULL)
{
  outcome <- NULL
  if(is.matrix(masking.matrix) & is.matrix(encoded.matrix))
  {
    print("")
    print(dim(masking.matrix))
    print(dim(encoded.matrix))
    masking.inverse  <- solve(masking.matrix)
    no.col           <- ncol(masking.inverse)
    no.row           <- nrow(encoded.matrix)
    outcome          <- matrix(rep(0,no.row * no.col),no.row, no.col)
    
     
   
    if (no.row == no.col)
    {
      outcome <- masking.inverse %*% encoded.matrix
      print(dim(outcome))
    }
  }
  
  return(outcome)
}

.compute.encoding.ratio <- function(decoded.matrix = NULL,param.name)
{
  outcome <- 0
  if (is.matrix(decoded.matrix))
  {
    if ((nrow(decoded.matrix) %% 2) == 1 &  (ncol(decoded.matrix) %% 2) == 1)
    {
      middle.row     <- ceiling(nrow(decoded.matrix)/2)
      middle.column  <- ceiling(ncol(decoded.matrix)/2)
      dot.product    <- decoded.matrix[middle.row, middle.column]
      param          <- get(param.name, pos=1)
      #encoding ratio = param/dot.product
      outcome        <- param/dot.product
    }
  }
  return(outcome)
}

#'@name encodeParameterDS
#'@title  encode a server parameter 
#'@description This server function encodes a given parameter using a dot product and two shared secrets.
#'@export
encodeParamDS <- function(param.name = NULL)
{
   outcome <- FALSE
   if(.is.param.valid(param.name) & .is.sharing.valid())
   {
       decoded.matrix  <- .decode.received.matrix(sharing$masking.matrix, sharing$received.matrix)
       encoding.ration <- .compute.encoding.ratio(decoded.matrix,param.name) 
       outcome <- TRUE 
   }
   return(outcome)
}




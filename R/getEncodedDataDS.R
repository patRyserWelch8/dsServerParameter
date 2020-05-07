#'@name getEncodedDataDS
#'@title  Retrieves the encoded data
#'@description This server function retrieves some encoded data to be passed onto the analysis computer
#'@export


.encode.data.with.sharing <- function()
{
    header       <- ""
    data         <- as.character(paste(sharing$encoded.matrix,sep="",collapse=","))
    size         <- as.integer(object.size(data)) #change to is numeric once parser is sorted ....
    no.columns   <- as.integer(ncol(sharing$encoded.matrix))
    timestamp    <- as.numeric(Sys.time())
    return.value <- list(header = "FM" , payload = data, property.a = size, property.b = no.columns, property.c = timestamp)
    return(return.value)
}

.encode.data.no.sharing <- function()
{
  header       <- ""
  data         <- as.character(paste(runif(11 *13, 100000, 400000),sep="", collapse=","))
  size         <- as.integer(object.size(data))
  no.columns   <- as.integer(runif(1, min=11, max=21))
  timestamp    <- as.numeric(Sys.time())
  return.value <- list(header = "FM" , payload = data, property.a = size, property.b = no.columns, property.c = timestamp) 
  return(return.value)
}


getEncodedDataDS <- function()
{
  #change value once new parser is solved....
  
  if(exists("sharing"))
  {
    sharing               <- get("sharing",pos = 1)
    encoded.matrix.exists <- "encoded.matrix" %in% names(sharing)
    no.columns            <- ncol(sharing$encoded.matrix)
    no.rows               <- nrow(sharing$encoded.matrix)
    
    
    if(encoded.matrix.exists & no.rows > 11 & no.columns > 13)
    {
      return.value <- .encode.data.with.sharing()
    }
    else
    {
      return.value <- .encode.data.no.sharing()
    }
  }
  else
  {
    return.value <- .encode.data.no.sharing()
  }
  return(return.value)
}

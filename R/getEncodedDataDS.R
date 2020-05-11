
.encode.data.with.sharing <- function()
{
    #remove conversion once new parsers is available
    header        <- ""
    data          <- as.character(paste(as.integer(sharing$encoded.matrix),sep="",collapse=","))
    size          <- as.integer(object.size(data)) #change to is numeric once parser is sorted ....
    no.columns    <- as.integer(ncol(sharing$encoded.matrix))
    timestamp     <- as.numeric(Sys.time())
    random.number <- as.numeric(runif(1,min = timestamp/15, max = timestamp - 200))
    return.value  <- list(header = "FM1" , 
                          payload = data, 
                          property.a = size, 
                          property.b = no.columns, 
                          property.c = timestamp, 
                          property.d = random.number)
    return(return.value)
}

.encode.data.no.sharing <- function()
{
  header        <- ""
  data          <- as.character(paste(runif(11 *13, 100000, 400000),sep="", collapse=","))
  size          <- as.integer(object.size(data))
  no.columns    <- as.integer(runif(1, min=11, max=21))
  timestamp     <- as.numeric(Sys.time())
  random.number <- ceiling(runif(1,min = timestamp/15, max = timestamp - 200))
  return.value  <- list(header = "FM2" , 
                        payload = data, 
                        property.a = size, 
                        property.b = no.columns, 
                        property.c = timestamp,
                        property.d = random.number)
  return(return.value)
}

#'@name getEncodedDataDS
#'@title  Retrieves the encoded data
#'@description This server function retrieves some encoded data to be passed onto the analysis computer
#'@export
getEncodedDataDS <- function()
{
  #change value once new parser is solved....
  
  if(exists("sharing",where=1))
  {
    sharing               <- get("sharing",pos = 1)
    encoded.matrix.exists <- "encoded.matrix" %in% names(sharing)
    

    if(encoded.matrix.exists)
    {
      no.columns            <- ncol(sharing$encoded.matrix)
      no.rows               <- nrow(sharing$encoded.matrix)
      if (no.rows >= 13 & no.columns >= 11)
      {
        #remove conversion once new parsers is available
        sharing$encoded.matrix <- as.integer(sharing$encoded.matrix)
       
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
  }
  else
  {
    return.value <- .encode.data.no.sharing()
  }
  return(return.value)
}

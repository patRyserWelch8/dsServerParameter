.encode.data.with.sharing <- function(encrypted.data, length, index)
{
    
    #remove conversion once new parsers is available
    header        <- ""
    data          <- as.character(paste(as.numeric(encrypted.data),sep="",collapse=";"))
    size          <- as.numeric(object.size(data)) 
    timestamp     <- as.numeric(Sys.time()) / size
    
  
    return.value  <- list(header = "FM1" , 
                          payload = data, 
                          property.a = size, 
                          property.b = length, 
                          property.c = timestamp, 
                          property.d = index/timestamp)
    return(return.value)
}

.encode.data.no.sharing <- function()
{
  header        <- ""
  data          <- as.character(paste(runif(100, 0.1, 1),sep="", collapse=";"))
  size          <- as.numeric(object.size(data))
  no.columns    <- as.integer(runif(1, min=settings$min_rows, max=settings$max_rows))
  no.rows       <- as.integer(runif(1, min=settings$min_columns, max=settings$max_columns))
  index         <- ceiling(runif(1, min = 0, max = 100))
  timestamp     <- as.numeric(Sys.time()) / size
  return.value  <- list(header = "FM2" , 
                        payload = data, 
                        property.a = size, 
                        property.b = as.integer(runif(1, min = 1, max = 6)), 
                        property.c = timestamp,
                        property.d = index/timestamp)
  return(return.value)
}


.encode.data.no.settings <- function()
{
  header        <- ""
  data          <- as.character(paste(runif(100, 0.1, 1),sep="", collapse=";"))
  size          <- as.numeric(object.size(data))
  no.columns    <- as.integer(runif(1, min=11, max=13))
  no.rows       <- as.integer(runif(1, min=13, max=15))
  index         <- runif(1, min = 0, max = 100)
  timestamp     <- as.numeric(Sys.time()) / size
  return.value  <- list(header = "FM3" , 
                        payload = data, 
                        property.a = size, 
                        property.b = as.integer(runif(1, min = 1, max = 6)), 
                        property.c = timestamp,
                        property.d = index/timestamp)
  return(return.value)
}



#'@name getCoordinatesDS
#'@title  Retrieves some data related to some coordinates used to encrypt the parameter
#'@description This server function retrieves some encrypted coordinates to be passed onto the analysis computer
#'@return a list made of a header, a payload, and four properties[a-d]
#'@details Some encoded data are transformed into a suitable format to be transferred from a DataSHIELD server to a DataSHIELD client. The property.a indicates
#'the size of the data at encoding time. Property b is the number of coordinates and property c a timestamp. property d a random number.
#'@seealso \link[dsParamServer]{assignDataDS} \link[dsParamServer]{getCoordinatesDS}
#'@export
getCoordinatesDS <- function()
{ 
   return.value <- .encode.data.no.settings()
   if(exists("settings",where=1))
   {
      return.value <- .encode.data.no.sharing()
      if(exists(settings$name.struct,where=1))
      {
        sharing      <- get(settings$name.struct,pos = 1)
        value.exists <- all(c(settings$index_x, settings$index_y) %in% names(sharing))
  
        if(value.exists)
        {
          print(sharing[[settings$index_x]])
          print(sharing[[settings$index_y]])
          data           <- c(sharing[[settings$index_x]],sharing[[settings$index_y]])
          no.params      <- length(sharing[[settings$index_x]])
          random.data    <- runif(100 - (2 * no.params), min = 0, max = 2)
          encrypted.data <- c(random.data[1:(length(random.data)/2)],
                              data,
                              random.data[((length(random.data)/2)+1):length(random.data)])
          index <- runif(1, min = 1, max= 100)
          return.value <- .encode.data.with.sharing(encrypted.data, length(sharing[[settings$index_x]]), index)
        }
      }  
    }
  
  return(return.value)
}


.compute.coordinates <- function()
{
  return.value <- encode.data.no.sharing()
  if(exists("settings",where = 1))
  {
    return.value <- encode.data.no.sharing()
    if(exists(settings$name.struct,where=1))
    {
      sharing      <- get(settings$name.struct,pos = 1)
      value.exists <- all(c(settings$index_x, settings$index_y) %in% names(sharing))
      
      if(value.exists)
      {
        data           <- c(sharing[[settings$index_x]],sharing[[settings$index_y]])
        no.params      <- length(sharing[[settings$index_x]])
        random.data    <- runif((settings$min_rows * settings$min_columns) + 1 - (2 * no.params), min = 0, max = 2)
        encrypted.data <- c(random.data[1:(length(random.data)/2)],
                            data,
                            random.data[((length(random.data)/2)+1):length(random.data)])
        index <- runif(1, min = 1, max= 100)
        return.value <- encode.data.with.sharing(encrypted.data, length(sharing[[settings$index_x]]), index)
      }
    }  
  }
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
  
  if (is.sharing.allowed())
  {
    if(!exists(settings$name.struct,where=1))
    {
      stop("SERVER::ERR::PARAM::003")
    }
    else
    {
      encoded.data <- .compute.coordinates()
      if(identical(encoded.data$header, "FM2"))
      {
          stop("SERVER::ERR::PARAM::004") 
      }
      else
      {
        return(encoded.data)
      }
    }
  }
  else
  {
    stop("SERVER::ERR::PARAM::001")
  }
  
  
  
}

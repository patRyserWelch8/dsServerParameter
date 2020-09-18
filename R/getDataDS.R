
.encode.data.with.sharing <- function(encrypted.data, no.columns, index)
{
    data          <- as.character(paste(as.numeric(encrypted.data),sep="",collapse=";"))
    size          <- as.numeric(object.size(data)) 
    timestamp     <- as.numeric(Sys.time()) / size
    
    return.value  <- list(header = "FM1" , 
                          payload = data, 
                          property.a = size, 
                          property.b = no.columns, 
                          property.c = timestamp, 
                          property.d = index/timestamp)
    return(return.value)
}

#- OBSOLETE this helper function returns a matrix made of random values. The exchange would 
#continue. However, the parameter exchange will not be correct. Its purpose is to
#provide a continuation throught the process, even if an error has occured.
.encode.data.no.sharing <- function()
{
  header        <- ""
  data          <- as.character(paste(runif(11 *13, 100000, 400000),sep="", collapse=";"))
  size          <- as.numeric(object.size(data))
  no.columns    <- as.integer(runif(1, min=settings$min_rows, max=settings$max_rows))
  no.rows       <- as.integer(runif(1, min=settings$min_columns, max=settings$max_columns))
  index         <- ceiling(runif(1, min = 0, max = no.columns))
  timestamp     <- as.numeric(Sys.time()) / size
  return.value  <- list(header = "FM2" , 
                        payload = data, 
                        property.a = size, 
                        property.b = no.columns, 
                        property.c = timestamp,
                        property.d = index/timestamp)
  return(return.value)
}

# OBSOLETE  - this helper function returns a matrix made of random values. The exchange would 
#continue. However, the parameter exchange will not be correct. Its purpose is to
#provide a continuation throught the process, even if an error has occured.
.encode.data.no.settings <- function()
{
  header        <- ""
  data          <- as.character(paste(runif(11 *13, 100000, 400000),sep="", collapse=";"))
  size          <- as.numeric(object.size(data))
  no.columns    <- as.integer(runif(1, min=11, max=13))
  no.rows       <- as.integer(runif(1, min=13, max=15))
  index         <- runif(1, min = 0, max = no.columns)
  timestamp     <- as.numeric(Sys.time()) / size
  return.value  <- list(header = "FM3" , 
                        payload = data, 
                        property.a = size, 
                        property.b = no.columns, 
                        property.c = timestamp,
                        property.d = index/timestamp)
  return(return.value)
}

.encode.encrypted.data <- function(master_mode = TRUE)
{
  sharing      <- get(settings$name.struct,pos = 1)
  
  if(!("encrypted" %in% names(sharing)))
  {
    stop("SERVER::ERR::PARAM::004")
  }
  else
  {
    no.columns            <- ncol(sharing[[settings$encrypted]])
    no.rows               <- ncol(sharing[[settings$encrypted]])
    
    #transpose has occured. So need to compare rows with columns settings 
    #and columns with rows settings
    if (no.columns >= settings$min_columns & 
        no.rows    >= settings$min_rows)
    {
      encrypted.data <- as.numeric(sharing[[settings$encrypted]])
      index <- runif(1, min =settings$min_rows, max= settings$max_rows)
      return(.encode.data.with.sharing(encrypted.data, no.columns, index))
    }
  }
}

#'@name getDataDS
#'@title  Retrieves the encrypted data from a server to the analysis computer
#'@description This server function retrieves some encrypted data to be passed onto the analysis computer
#'@param master_mode Boolean argument. It indicates the mode of a server is a \strong{master} or a \strong{receiver}. By default, set to TRUE.
#'@return a list made of a header, a payload, and four properties[a-d]
#'@details Some encrypted data are transformed into a suitable format to be transferred from a DataSHIELD server to a DataSHIELD client. The property.a indicates
#'the size of the data at encoding time. Property b is the number of columns and property c a timestamp. property d a random number.
#'@seealso \link[dsParamServer]{assignDataDS}
#'@export
getDataDS <- function(master_mode = TRUE)
{ 
   if(!exists("settings",where=1))
   {
     stop("SERVER::ERR::PARAM::002")
   }
   else
   {
     if(!settings$sharing.allowed)
     {
       stop("SERVER::ERR::PARAM::001")
     }
     else
     {
      if(!exists(settings$name.struct,where=1))
      {
        stop("SERVER::ERR::PARAM::003")
      }
      else
      {
        return(.encode.encrypted.data(master_mode))
      }
    }
  }
}

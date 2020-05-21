
.encode.data.with.sharing <- function(encrypted.data, no.columns, index)
{
    #remove conversion once new parsers is available
    header        <- ""
    data          <- as.character(paste(as.integer(encrypted.data),sep="",collapse=","))
    size          <- as.integer(object.size(data)) #change to is numeric once parser is sorted ....
    timestamp     <- as.numeric(Sys.time()) / size
    print(index)
  
    return.value  <- list(header = "FM1" , 
                          payload = data, 
                          property.a = size, 
                          property.b = no.columns, 
                          property.c = timestamp, 
                          property.d = index/timestamp)
    return(return.value)
}

.encode.data.no.sharing <- function()
{
  header        <- ""
  data          <- as.character(paste(runif(11 *13, 100000, 400000),sep="", collapse=","))
  size          <- as.integer(object.size(data))
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

#'@name getDataDS
#'@title  Retrieves the encrypted data from a server to the analysis computer
#'@description This server function retrieves some encrypted data to be passed onto the analysis computer
#'@export
getDataDS <- function(master_mode = TRUE)
{
  
  if (exists("settings",where=1))
  {  
      if(exists(settings$name.struct,where=1))
      {
        sharing      <- get(settings$name.struct,pos = 1)
        value.exists <- "encrypted" %in% names(sharing)
  
        if(value.exists)
        {
          no.columns            <- ncol(sharing[[settings$encrypted]])
          no.rows               <- nrow(sharing[[settings$encrypted]])
          if (no.rows >= 13 & no.columns >= 11)
          {
            #remove conversion once new parsers is available
            encrypted.data <- as.integer(sharing[[settings$encrypted]])
            if(master_mode)
            {
              return.value <- .encode.data.with.sharing(encrypted.data, no.columns, sharing[[settings$index_y]])
            }
            else
            {
              return.value <- .encode.data.with.sharing(encrypted.data, no.columns, sharing[[settings$index_x]])
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
      }
      else
      {
        return.value <- .encode.data.no.sharing()
      }
  }
  return(return.value)
}


.save.coordinates <- function(received.data = NULL, no.params)
{
  #if(exists("settings", where =1))
  #{
    if (is.vector(received.data))
    {
      sharing <- list()
      if (exists("sharing", where=1))
      {
        sharing = get("sharing", pos = 1)
      }
      
      no.values <- length(received.data)
      
      if (no.values == (2 * no.params))
      {
        sharing[[settings$index_x]] <- received.data[1:no.params]
        sharing[[settings$index_y]] <- received.data[(no.params+1):no.values]
        assign(settings$name.struct, sharing, pos = 1)
      }
    }
  #}
}

.create.data <- function(data = NULL,  no.params = 1)
{
  received.data <- rep(0,4)
  if (is.character(data) & is.numeric(no.params))
  {
    can.be.converted <- grepl('^-?[0-9.;e]+$', data)
   
    if(can.be.converted)
    {
      data.list       <- strsplit(data,";")
      if (length(data.list[[1]]) > 1)
      {
          data.vector    <- unlist(data.list)
          data.numeric   <- as.numeric(data.vector)
          middle.data    <- (length(data.numeric) - (2 * no.params))/2
          received.data  <- data.numeric[(middle.data+1):((length(data.numeric)-middle.data))]
      }
    }
  }
  return(received.data)
}

.is.assigned.coordinates.correct <- function()
{
  outcome <- FALSE
  if(exists("settings", where =1))
  {
      if (exists(settings$name.struct,where=1))
      {
        sharing       <- get(settings$name.struct,pos=1)
        structure     <- c(settings$index_x,settings$index_y)
       
        total.correct <- sum(structure %in% names(sharing))
        value.exists  <- length(structure) ==  total.correct
        
        if (value.exists)
        {
          outcome <- is.vector(sharing[[settings$index_x]]) & is.vector(sharing[[settings$index_y]])
        }
      }
  }
  return(outcome)
}

.assignCoordinates <- function(header = "", payload = "", property.a = 0, 
                               property.b = 0, property.c = 0.0, property.d = 0.0)
{
  received.data  <- .create.data(payload,property.b)
  .save.coordinates(received.data, property.b)
  outcome          <- .is.assigned.coordinates.correct()
}



#'@name assignCoordinatesDS
#'@title  assign data to one or multiple servers with some encrypted data from the analysis computer INCORRECT 
#'@description This server function assigns some values into a specific structure. INCORRECT
#'@param header character argument. Header information received from another server.
#'@param payload  character argument. Payload information received from another server. 
#'@param property.a numeric argument. Property.a received from another server. 
#'@param property.b numeric argument. Property.b received from another server. 
#'@param property.c numeric argument. Property.c received from another server. 
#'@param property.d numeric argument. Property.d received from another server. 
#'@details Some data are being assign into a specific structure used to share parameter in some privacy-protection settings. The process used by 
#'\link[dsParamServer]{getDataDS} is reversed. 
#'@seealso \link[dsParamServer]{getDataDS}
#'@export


assignCoordinatesDS <- function(header = "", payload = "", property.a = 0, 
                              property.b = 0, property.c = 0.0, property.d = 0.0)
{
  outcome <- FALSE
  if (is.sharing.allowed())
  {
    if ( is.character(header) & is.character(payload)
       & is.numeric(property.a) &  is.numeric(property.b) 
       & is.numeric(property.c) & is.numeric(property.d))
       {
          if (nchar(header) > 0 & nchar(payload) > 0 & property.a > 0 
             & property.b > 0 & property.c > 0 & property.d > 0)
            {
               return(.assignCoordinates(header,payload,property.a, property.b, property.c, property.d))
            }
            else
            {
               stop("SERVER::ERR::PARAM::006")
            }
        }
        else
        {
          stop("SERVER::ERR::PARAM::005")
        }
  }
  else
  {
    stop("SERVER::ERR::PARAM::001")
  }
  return(outcome)
}

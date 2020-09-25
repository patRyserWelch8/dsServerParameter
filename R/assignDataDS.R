
.save <- function(received.matrix = NULL, master_mode)
{ 
    if (is.matrix(received.matrix))
    {
      sharing <- list()
      if (exists("sharing", where=1))
      {
        sharing = get("sharing", pos = 1)
      }
      sharing[[settings$received]] <- received.matrix
      assign(settings$name.struct, sharing, pos = 1)
    }
}

.create.matrix <- function(data = NULL,  no.columns = 1)
{
  numbers         <- rep(x = 0, times=4)
  received.matrix <- matrix(as.numeric(numbers),2,2)
  
  
  if (is.character(data) & is.numeric(no.columns))
  {
    can.be.converted <- grepl('^-?[0-9.;e]+$', data)
    if(can.be.converted)
    {
      data.list       <- strsplit(data,";")
      length(data.list)
      
      if (length(data.list[[1]]) > 1)
      {
        
          data.vector <- unlist(data.list)
          data.vector <- gsub(" ", "",data.vector)
          no.rows     <- length(data.vector)/no.columns

      
          if (no.rows > 1 & no.columns > 1)
          {
            
              data.numeric    <- as.numeric(x = data.vector)
              received.matrix <- matrix(data=data.numeric,nrow=no.rows, ncol= no.columns)
          }
      }
    }
  }
  return(received.matrix)
}

.is.assigned.values.correct <- function(master_mode)
{
  outcome <- FALSE
  if (exists(settings$name.struct,where=1))
  {
    sharing       <- get(settings$name.struct,pos=1)
    structure     <- c(settings$received)
   
    total.correct <- sum(structure %in% names(sharing))
    value.exists  <- length(structure) ==  total.correct
    
    if (value.exists)
    {
      outcome <- is.matrix(sharing[[settings$received]])
    }
  }
  return(outcome)
}

.assignData <- function(master_mode = TRUE, header = "", payload = "", property.a = 0, 
              property.b = 0, property.c = 0.0, property.d = 0.0)
{
  
  received.matrix  <- .create.matrix(payload,property.b)
  .save(received.matrix, master_mode)
  return(.is.assigned.values.correct(master_mode))
}

#'@name assignDataDS
#'@title  assign data to one or multiple servers with some encrypted data from the analysis computer
#'@description This server function assigns some values into a specific structure.
#'@param master_mode Boolean argument. It indicates the mode of a server is a \strong{master} or a \strong{receiver}. By default, set to TRUE.
#'@param header character argument. Header information received from another server.
#'@param payload  character argument. Payload information received from another server. 
#'@param property.a numeric argument. Property.a received from another server. 
#'@param property.b numeric argument. Property.a received from another server. 
#'@param property.c numeric argument. Property.a received from another server. 
#'@param property.d numeric argument. Property.a received from another server. 
#'@details Some data are being assign into a specific structure used to share parameter in some privacy-protection settings. The process used by 
#'\link[dsParamServer]{getDataDS} is reversed. 
#'@seealso \link[dsParamServer]{getDataDS}
#'@export
assignDataDS <- function(master_mode = TRUE, header = "", payload = "", property.a = 0, 
                              property.b = 0, property.c = 0.0, property.d = 0.0)
{
  
  if (is.sharing.allowed())
  {
    if ( is.character(header) & is.character(payload)
         & is.numeric(property.a) &  is.numeric(property.b) 
         & is.numeric(property.c) & is.numeric(property.d))
    {
      if (nchar(header) > 0 & nchar(payload) > 0 & property.a > 0 
          & property.b > 0 & property.c > 0 & property.d > 0)
      {
        return(.assignData(master_mode,header, payload,property.a, property.b, property.c, property.d))
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
}

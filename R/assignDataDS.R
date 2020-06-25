#index is not needed now 
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

#not required 
.compute.index <- function(encrypted.index,timestamp)
{
  index <- ceiling(runif(1, min= settings$max_columns+3, max = settings$max_columns+103))
  if (is.numeric(encrypted.index))
  {
     index <- floor(encrypted.index * timestamp)
  }
  return(index)
}

.create.matrix <- function(data = "",  no.columns = 1)
{
  received.matrix <- matrix (rep(0),2,2)
  
  if (is.character(data) & is.integer(no.columns))
  {
   
    data.list       <- strsplit(data,",")
    data.vector     <- unlist(data.list)
    data.numeric    <- as.numeric(data.vector)
    no.rows         <- length(data.numeric)/no.columns
    
    
    if (no.rows > 1 & no.columns > 1)
    {
      received.matrix <- matrix(data=data.numeric,nrow=no.rows, ncol= no.columns)
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
  outcome <- FALSE
  if ( is.character(header) & is.character(payload)
     & is.numeric(property.a) &  is.numeric(property.b) 
     & is.numeric(property.c) & is.numeric(property.d))
     {
        if (nchar(header) > 0 & nchar(payload) > 0 & property.a > 0 
           & property.b > 0 & property.c > 0 & property.d > 0)
          {
            received.matrix  <- .create.matrix(payload,property.b)
            #index            <- .compute.index(property.d, property.c)
            .save(received.matrix, master_mode)
            outcome          <- .is.assigned.values.correct(master_mode)
          }
     }
  return(outcome)
}

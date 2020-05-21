
.save <- function(received.matrix = NULL, index = settings$min_columns + 209, master_mode)
{
    if (is.matrix(received.matrix) & is.numeric(index))
    {
      if (exists("sharing", where=1))
      {
        sharing = get("sharing", pos = 1)
      }
      else
      {
        sharing <- list()
      }
      
      sharing[[settings$received]] <- received.matrix
      if(master_mode)
      {
        sharing[[settings$index_x]] <- index
        
      }
      else
      {
        #receiver assignment: transpose in master taken into account
        sharing[[settings$index_y]]    <- index
      }
      assign(settings$name.struct, sharing, pos = 1)
    }
}

.compute.index <- function(encrypted.index,timestamp)
{
  index <- ceiling(runif(1, min= settings$max_columns+3, max = settings$max_columns+103))
  if (is.numeric(encrypted.index))
  {
     index <- as.integer(encrypted.index * timestamp)
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
    if(master_mode)
    {
      structure     <- c(settings$received,settings$index_x)
    }
    else
    {
      structure     <- c(settings$received,settings$index_y)
    }
   
    total.correct <- sum(structure %in% names(sharing))
    value.exists  <- length(structure) %in%  total.correct
    
    if (value.exists)
    {
      if(master_mode)
      {
        outcome <- is.numeric(sharing[[settings$index_x]])
      }
      else
      {
        outcome <- is.numeric(sharing[[settings$index_y]])
      }
    }
  }
  return(outcome)
}

#'@name assignDataDS
#'@title  assign data to one or multiple servers with some encrypted data from the analysis computer
#'@description This server function assigns some values into a specific structure.
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
            index            <- .compute.index(property.d, property.c)
            .save(received.matrix, index, master_mode)
            outcome          <- .is.assigned.values.correct(master_mode)
          }
     }
  return(outcome)
}

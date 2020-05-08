#'@name sendEncodedDataDS
#'@title  decodes some data and transform them into a matrix
#'@description This server function assigns some values into a specific structure.
#'@export


sendEncodedDataDS <- function(header = "", payload = "", property.a = 0, 
                              property.b = 0, property.c = 0.0, property.d = 0.0)
{
  outcome <- FALSE
  if (is.character(header) & is.character(payload) & is.integer(property.a) &  is.integer(property.b) 
      & is.numeric(property.c) & is.numeric(property.d))
  {
   
    received.matrix <- .create.matrix(payload,property.b)
    .save.matrix(received.matrix)
    outcome <- .is.matrix.created()
  }
  return(outcome)
}

.save.matrix <- function(received.matrix = NULL)
{
    if (is.matrix(received.matrix))
    {
      if (!exists("sharing", where=1))
      {
        sharing <- list()
      }
      else
      {
        sharing = get("sharing", pos = 1)
      }
      
      sharing$received.matrix <- received.matrix
      assign("sharing", sharing, pos = 1)
    }
}

.create.matrix <- function(data = "",  no.columns = 1)
{
  received.matrix <- matrix (rep(0),2,2)
  
  if (is.character(data) & is.integer(no.columns))
  {
    data.list       <- strsplit(data,",")
    data.vector     <- unlist(data.list)
    data.numeric    <- as.numeric(data.vector)
    no.rows          <- length(data.numeric)/no.columns
    

    if (no.rows > 1 & no.columns > 1)
    {
      received.matrix <- matrix(data=data.numeric,nrow=no.rows, ncol= no.columns)
    }
  }
    return(received.matrix)
}


.is.matrix.created <- function()
{
  outcome <- FALSE

  if (exists("sharing",where=1))
  {
    sharing <- get("sharing",pos=1)
    received.matrix.exists <- ("received.matrix" %in% names(sharing))
    if (received.matrix.exists)
    {
      outcome <- is.matrix(sharing$received.matrix)
    }
  }
  return(outcome)
}


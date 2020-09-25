#'@name is.sharing.allowed
#'@title  verifies the variables used to set the parametrisation to for sharing parameters 
#'exists on a DataSHIELD server.
#'@description This server function checks some settings used exchange parameters between 
#'DataSHIELD server exists. It also verifies the data owners and governance have allowed the 
#'sharing of parameters in a specific server.
#'@details This is a helper function. It cannot be called directly from any client-side
#'function.
#'
is.sharing.allowed <- function()
{
  outcome <- FALSE
  if (exists("settings",where=1))
  {
    if(settings$sharing.allowed)
    {
      outcome <- TRUE
    }
  }
  return(outcome)
}

#'@name encode.data.with.sharing
#'@title  encode some obscured data to be exchanged from one server to another.
#'@description This server function can only be used with some encrypted data. It 
#'format the data prior its transfer to a client-side function. 
#'@param encrypted.data - data to be encoded
#'@param length - an indication how long should be the data
#'@param index - A random number related to the data
#'@details This is a helper function. It cannot be called directly from any client-side
#'function.
#'@seealso \link[dsServerParameter]{getDataDS}, \link[dsServerParameter]{getCoordinatesDS},
#'\link[dsServerParameter]{encode.data.no.sharing}
encode.data.with.sharing <- function(encrypted.data, length, index)
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

#'@name encode.data.no.sharing
#'@title  encode some randomised data
#'@description This server-side function generates some random data to be made available to a client-side
#'function. Its purpose is to mimick the same behaviour as [dsServerParameter]{encode.data.with.sharing}. It aims a 
#'a "decoy", if an error has occurred in the process.
#'
#'@details This is a helper function. It cannot be called directly from any client-side
#'function.
#'@seealso \link[dsParamServer]{getDataDS} \link[dsParamServer]{getCoordinatesDS},[dsParamServer]{encode.data.no.sharing}
encode.data.no.sharing <- function()
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


are.params.created <- function(param_names = c())
{
  params.exist     <- FALSE
  all.numeric <- FALSE
  
  if (length(param_names) > 0)
  {
    if(length(param_names) >=1  & is.character(param_names) )
    {
      list.var <- ls(pos = 1)
      params.exist  <- all(param_names %in% list.var)
      if(params.exist)
      {
        params <-  mget(x = param_names, envir = as.environment(1))
        all.numeric <- all(sapply(params, is.numeric))
      }
    }
  }
  return(params.exist & all.numeric)
}

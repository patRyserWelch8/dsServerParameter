.get.encoded.param <- function()
{
  outcome <- list()
  
  if(exists(settings$name.struct, where =1))
  {
    if (is.list(get(settings$name.struct, pos=1)))
    {
      outcome <- get(settings$name.struct, pos=1) 
    }
  }
  return(outcome)
}

.is.encoded.param.valid <- function(encoded.param = NULL)
{
  correct <- FALSE
  expected.list <- c(settings$encrypted,settings$masking,settings$received, settings$decrypted, 
                     settings$index_x, settings$index_y)
 
  if (is.list(encoded.param))
  {
    list.attributes <- names(encoded.param)
    attributes.exist <- list.attributes %in% expected.list
    total.correct = sum(attributes.exist == TRUE)
    correct <- (total.correct == length(expected.list))
  }
  return(correct)
}

.create.vector.params <- function (param_names = "")
{
  outcome <- c()
  names.list <- strsplit(param_names,";")
  outcome <- unlist(names.list)
  return(outcome)
}

.decryptParam <- function(param_names = NULL, tolerance = 8)
{
  outcome <- FALSE
  param.value <- NA
  
  params    <- .create.vector.params(param_names)
  no.params <- length(params)
  rows      <- ceiling(sharing[[settings$index_x]] * sharing[[settings$no_columns]])
  columns   <- ceiling(sharing[[settings$index_y]] * sharing[[settings$no_rows]])
  
  #those are swapped due to transpose in encoding process
  for(index in 1:no.params)
  {
    param_name  <- params[index]
    param.value <-  round(sharing$decrypted[columns[index],rows[index]], tolerance)
    assign(param_name,param.value, pos = 1)
  }
  outcome <- !is.na(param.value)
  return(outcome)
}

#'@name decryptParamDS
#'@title  decrypt a server parameter 
#'@description This server function decrypts a given parameter in matrix.
#'@param param_names character argument. Name of the variable used  to store the parameter value on a server.
#'@param tolerance numerical argument. Number of decimal places aimed to used for accuracy
#'@export
decryptParamDS <- function(param_names = NULL, tolerance = 8)
{
   outcome <- FALSE
   param.value <- NA
   
   if (is.sharing.allowed())
   {
     sharing <- .get.encoded.param()
     if(.is.encoded.param.valid(sharing))
     {   
       return(.decryptParam(param_names, tolerance))
     }
     else
     {
       stop("SERVER::ERR::PARAM::009")
     }   
   }
   else
   {
     stop("SERVER::ERR::PARAM::001")
   }
  
}




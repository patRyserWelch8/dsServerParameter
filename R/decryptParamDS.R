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
  expected.list <- c(settings$encrypted,settings$masking,settings$received, settings$decrypted, settings$index_x, settings$index_y)
 
  if (is.list(encoded.param))
  {
    list.attributes <- names(encoded.param)
    attributes.exist <- list.attributes %in% expected.list
    total.correct = sum(attributes.exist == TRUE)
    correct <- (total.correct == length(expected.list))
  }
  return(correct)
}

#'@name decryptParamDS
#'@title  decrypt a server parameter 
#'@description This server function decrypts a given parameter in matrix.
#'@param param_name character argument. Name of the variable used  to store the parameter value on a server.
#'@export
decryptParamDS <- function(param_names = NULL)
{
   outcome <- FALSE
   param.value <- NA
   if (exists("settings", where = 1))
   {
     sharing <- .get.encoded.param()
    
     if(.is.encoded.param.valid(sharing))
     { 
         no.params <- length(param_names)
         rows      <- ceiling(sharing[[settings$index_y]] * sharing[[settings$no_columns]])
         columns   <- ceiling(sharing[[settings$index_y]] * sharing[[settings$no_columns]])
         for (index in 1:no.params)
         {
           param_name <- param_names[index]
           param.value <-  sharing$decrypted[rows[index],columns[index]]
           print(param.value)
           assign(param_name,param.value, pos = 1)
         }
     }
   }
   outcome <- !is.na(param.value)
   return(outcome)
}




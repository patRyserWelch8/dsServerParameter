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
  expected.list <- c(settings$encrypted,settings$masking,settings$received, settings$decrypted)
 
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
decryptParamDS <- function(param_name = NULL)
{
   outcome <- FALSE
   param.value <- NA
   if (exists("settings", where = 1))
   {
    
     sharing <- .get.encoded.param()
    
     if(.is.encoded.param.valid(sharing))
     { 
         print(3)
         #decrypt the received matrix: shared secret
         #decrypted.matrix               <- .decrypt.received.matrix(sharing[[settings$masking]], sharing[[settings$received]]) 
         #column                         <- sharing[[settings$index_x]]
         #row                            <- sharing[[settings$index_y]]
         #address this once columun and row are communicated ......
         column = 3
         row = 3
         param.value <-  sharing$decrypted[column,row]
         assign(param_name,param.value, pos = 1)
     }
   }
   outcome <- !is.na(param.value)
   return(outcome)
}




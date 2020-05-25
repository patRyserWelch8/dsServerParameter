.get.shared.secrets <- function()
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

.is.shared.secrets.valid <- function(shared.secrets)
{
  correct <- FALSE
  expected.list <- c(settings$encrypted,settings$masking,settings$received, settings$decrypted)
 
  if (is.list(shared.secrets))
  {
    list.attributes <- names(shared.secrets)
    attributes.exist <- list.attributes %in% expected.list
    total.correct = sum(attributes.exist == TRUE)
    correct <- (total.correct == length(expected.list))
  }
  return(correct)
}

.is.param.valid <- function(param.name = NULL)
{
  #use existsDS...
  outcome <- FALSE
 
  if(is.character(param.name))
  {
    if(exists(param.name,where=1))
    {
      
      param <- get(param.name, pos=1)
      if(is.numeric(param))
      { 
        outcome <- TRUE
      }
    }
  }
  return(outcome)
}



.compute.encoding.ratio <- function(decrypted.matrix = NULL,param.name, column, row)
{
  outcome <- 0
  print(ncol(decrypted.matrix))
  if (is.matrix(decrypted.matrix) & is.character(param.name))
  {
    if (exists(param.name, where = 1))
    {
      param <- get(param.name, pos=1)
      
      if ((nrow(decrypted.matrix) %% 2) == 1 &  (ncol(decrypted.matrix) %% 2) == 1)
      {
       
        dot.product    <- decrypted.matrix[row, column]
        outcome        <- param/dot.product
      }
    }
  }
  return(outcome)
}



.encrypt.parameter <- function(concealing.matrix = NULL, column = 0, encoding.ratio=NULL)
{
  outcome <- 0
  if(is.matrix(concealing.matrix) & is.numeric(column)  & is.numeric(encoding.ratio))
  {
    if (column > 0 & column <= ncol(concealing.matrix))
    {
      outcome <- encoding.ratio * concealing.matrix[,column]
    }
  }
  return(outcome)
}

.is.encrypted.structure.valid <- function(expected.list)
{
  correct <- FALSE
  
  if(exists("sharing",where=1))
  {
    
   
    sharing       <- get("sharing",pos=1)
    
    if (is.list(sharing))
    {
      list.attributes  <- names(sharing)
      attributes.exist <- list.attributes %in% expected.list
      total.correct    <- sum(attributes.exist == TRUE)
      correct          <- (total.correct == length(expected.list))
    }
  }
  return(correct)
}


#'@name encryptParamDS
#'@title  encrypt a server parameter 
#'@description This server function encrypts a given parameter using a dot product and two shared secrets.
#'@export
encryptParamDS <- function(param.name = NULL)
{
   outcome <- FALSE
   if (exists("settings", where = 1))
   {
     sharing <- .get.shared.secrets()
     print(.is.param.valid(param.name) & .is.shared.secrets.valid(sharing))
     if(.is.param.valid(param.name) & .is.shared.secrets.valid(sharing))
     {
       
         #decrypt the received matrix: shared secret
         #decrypted.matrix               <- .decrypt.received.matrix(sharing[[settings$masking]], sharing[[settings$received]]) 
         #column                         <- sharing[[settings$index_x]]
         #row                            <- sharing[[settings$index_y]]
         print(sharing[[settings$decrypted]])
         encoding.ratio                 <- .compute.encoding.ratio(sharing[[settings$decrypted]],
                                                                   param.name,
                                                                   column = sharing[[settings$index_x]], 
                                                                   row = sharing[[settings$index_y]]) 
         
        
         #decrypt encrypted matrix to find concealed values: shared secret
         concealing.matrix               <- t(solve(t(sharing[[settings$masking]])) %*% sharing[[settings$encrypted]])
         sharing[[settings$data]]       <- .encrypt.parameter(concealing.matrix,column = sharing[[settings$index_x]],encoding.ratio)
         expected.list                  <- c(settings$data,settings$index_x, settings$index_y, settings$encrypted, settings$masking)
         assign(settings$name.struct,sharing, pos=1)
         outcome <- .is.encrypted.structure.valid(expected.list)
         
     }
   }
   return(outcome)
}




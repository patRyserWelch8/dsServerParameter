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

.is.encrypted.structure.valid <- function()
{
  correct <- FALSE
  
  if(exists("sharing",where=1))
  {
      sharing       <- get("sharing",pos=1)
      
      if (is.list(sharing))
      {
        list.attributes  <- names(sharing)
        correct <- settings$data %in% list.attributes  
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
     if(.is.param.valid(param.name) & .is.shared.secrets.valid(sharing))
     {
       
         #decrypt the received matrix: shared secret
         #decrypted.matrix               <- .decrypt.received.matrix(sharing[[settings$masking]], sharing[[settings$received]]) 
         #column                         <- sharing[[settings$index_x]]
         #row                            <- sharing[[settings$index_y]]
         #address this once columun and row are communicated ......
         column = 3
         row = 3
         encoding.ratio                 <- .compute.encoding.ratio(sharing[[settings$decrypted]],
                                                                   param.name,
                                                                   column = column, 
                                                                   row = column) 
   
         #decrypt encrypted matrix to find concealed values: shared secret
         concealing.matrix              <- t(solve(t(sharing[[settings$masking]])) %*% sharing[[settings$encrypted]])
         sharing[[settings$data]]       <- .encrypt.parameter(concealing.matrix,column = column,encoding.ratio)
         assign(settings$name.struct,sharing, pos=1)
         outcome <- .is.encrypted.structure.valid()
         
     }
   }
   return(outcome)
}




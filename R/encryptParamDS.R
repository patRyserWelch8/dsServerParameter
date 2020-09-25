.get.shared.secrets <- function()
{
  outcome <- list()
  if(exists(settings$name.struct, where = 1))
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
  expected.list <- c(settings$encrypted,settings$masking,settings$received, 
                     settings$decrypted, settings$index_x, settings$index_y, settings$param_names)
 
  if (is.list(shared.secrets))
  {
    list.attributes <- names(shared.secrets)
    attributes.exist <- list.attributes %in% expected.list
    total.correct = sum(attributes.exist == TRUE)
    correct <- (total.correct == length(expected.list))
  }
  return(correct)
}

.is.param.valid <- function(param_name = NULL)
{
  #use existsDS...
  outcome <- FALSE
 
  if(is.character(param_name))
  {
    if(exists(param_name,where=1))
    {
      param <- get(param_name, pos=1)
      if(is.numeric(param))
      { 
        outcome <- TRUE
      }
    }
  }
  return(outcome)
}

.compute.encoding.ratio <- function(decrypted.matrix = NULL,param_name, column, row)
{
  outcome <- 0
  
  if (is.matrix(decrypted.matrix) & is.character(param_name))
  {
    if (exists(param_name, where = 1))
    {
      param <- get(param_name, pos=1)
      
      if ((nrow(decrypted.matrix) %% 2) == 1 &  (ncol(decrypted.matrix) %% 2) == 1)
      {
       
        dot.product    <- decrypted.matrix[row, column]
        outcome        <- param/dot.product
      }
    }
  }
  return(outcome)
}

.encrypt <- function(concealing.matrix = NULL, column = 0, encoding.ratio=NULL)
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

.compute.concealing.matrix <- function()
{
  return(t(solve(t(sharing[[settings$masking]])) %*% sharing[[settings$encrypted]]))
}

.encrypt.param <- function(index,concealing.matrix)
{
  param_name <- sharing[[settings$param_names]][index]
  if(.is.param.valid(param_name))
  {
    column         <- ceiling(sharing[[settings$index_x]][index] * ncol(sharing[[settings$decrypted]]))
    row            <- ceiling(sharing[[settings$index_y]][index] * nrow(sharing[[settings$decrypted]]))
    encoding.ratio <- .compute.encoding.ratio(sharing[[settings$decrypted]], param_name, column, row)
    #column is becomes the index of the row. This is guided by the transpose when the concealing matrix is encrypted
    data           <- .encrypt(concealing.matrix,column=row,encoding.ratio)
  }
}

.complete.encryption <- function(sharing)
{
  outcome <- FALSE
  
  if(.is.shared.secrets.valid(sharing))   #.is.param.valid(param_name) &
  {
    #decrypt encrypted matrix to find concealed values: shared secret         
    #concealing.matrix <- .compute.concealing.matrix() 
    no.params <- length(sharing[[settings$param_names]])
    data <- list()
    for (index in 1:no.params)
    {
      data[[index]] <- .encrypt.param(index,sharing[[settings$concealing]])
    }
    
    sharing[[settings$data]] <- data
    assign(settings$name.struct,sharing, pos=1)
    outcome <- .is.encrypted.structure.valid()
  }
  return(outcome)
}

#'@name encryptParamDS
#'@title  encrypt a server parameter 
#'@description This server function encrypts a given parameter using a dot product and two shared secrets.
#'@export
encryptParamDS <- function() 
{
   
   if (is.sharing.allowed() )
   { 
     sharing <- .get.shared.secrets()
     if(are.params.created(sharing[[settings$param_names]]))
     {
       return(.complete.encryption(sharing))
     }
     else
     {
       stop("SERVER::ERR::PARAM::008")
     }
   }
   else
   {
     stop("SERVER::ERR::PARAM::001")
   }
   
}




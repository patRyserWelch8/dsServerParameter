.has.correct.data <- function(param_names = c())
{
  
  params.exist     <- FALSE
  encrypted.exists <- FALSE
  if (exists("settings",where = 1)  &
      !is.null(param_names))
  {
   
    if (exists(settings$name.struct,where = 1))
    {
     
      if(length(param_names) >= 1 & is.character(param_names) )
      {
       
        list.files <- ls(pos = 1)
        params.exist  <- all(param_names %in% list.files)
      }

      sharing           <- get(settings$name.struct, pos = 1)
      encrypted.exists  <- settings$encrypted %in% names(sharing)
    }
  }
  
  return(encrypted.exists & params.exist)
}

.generate.ratios <- function(no.elements = 0, coordinate = 0)
{
  stop    <-  no.elements == 0 & coordinate == 0
 
  while(!stop)
  {
     outcome   <- as.vector(runif(no.elements, min = 0.01, max = 0.95))
     values    <- ceiling(outcome * coordinate)
     no.levels <- length(levels(factor(values)))
     stop <- (no.levels == no.elements)
  }
  
  return(outcome)
}

.init.coordinates.ratios <- function(param_names, sharing)
{
  if(is.list(sharing))
  {
    sys.time <- as.numeric(Sys.time())
    set.seed(sys.time)
    random.number <- runif (1, min = 1, max = 10^6)
    
    set.seed(sys.time/random.number)
    sharing[[settings$index_x]]     <- .generate.ratios(no.elements = length(param_names), coordinate = sharing[[settings$no_columns]])
    sharing[[settings$index_y]]     <- .generate.ratios(no.elements = length(param_names), coordinate = sharing[[settings$no_rows]])
    sharing[[settings$param_names]] <- param_names

  
    return(sharing)
  }
  else
  {
    return(list())
  }
}

.is.outcome.valid <- function(sharing, expected.fields)
{
  correct <- FALSE
  
  if (is.list(sharing) & is.vector(expected.fields))
  {
    list.attributes <- names(sharing)
    attributes.exist <- list.attributes %in% expected.fields
    total.correct = sum(attributes.exist == TRUE)
    correct <- total.correct == length(expected.fields)
  }
  
  return(correct)
}

#'@name   assignParamSettingsDS
#'@title  assigns some settings used to encrypt and decrypt the parameters
#'@description This server function sets some settings specific to the parameters encryption and decryption mechanisms. 
#'The latter should identify a column and row for each parameter in some matrices. The row and column is disclosive. So, it remains
#'on the server and cannot be analysed directly.
#'@param param_names  character vector. Name of the server parameter to encrypt.
#'@export
assignParamSettingsDS <- function(param_names = c())
{
  outcome <-FALSE
 
  if (.has.correct.data(param_names))
  {
  
    sharing <- get(settings$name, pos = 1)
    sharing <-.init.coordinates.ratios(param_names, sharing)
    expected.fields <- c(settings$index_x, settings$index_y, settings$param_names)
    outcome <- .is.outcome.valid(sharing,expected.fields)
    
    if(outcome)
    {
      assign(settings$name, sharing, pos = 1)
    }
  }
  return(outcome)
}

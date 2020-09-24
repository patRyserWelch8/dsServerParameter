.get.sharing.data <- function()
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

.is.cleaned.structure.valid <- function(expected.list)
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

#'@name removeEncryptingDataDS
#'@title Remove data used to encrypt some parameters
#'@description This server function can be used to remove the data used to encrypt a parameter between
#'a two servers
#'@export
removeEncryptingDataDS <- function(master_mode = TRUE)
{
  outcome <- FALSE
  if(is.sharing.allowed())
  {
    sharing                        <- .get.sharing.data()
    if(master_mode)
    {
        expected.list               <- c(settings$data,settings$no_columns, settings$no_rows, settings$index_x, 
                                            settings$index_y,settings$param_names)
    }
    else
    {
      expected.list                  <- c(settings$concealing,settings$no_columns, settings$no_rows, settings$index_x, 
                                          settings$index_y)
    }
   
    assign("sharing", sharing,pos=1)
    outcome <- .is.cleaned.structure.valid(expected.list)
  }
  else
  {
    stop("SERVER::ERR::PARAM::001")
  }
  return(outcome)
}

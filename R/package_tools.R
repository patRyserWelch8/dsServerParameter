#'@name is.sharing.allowed
#'@title  verifies the variables used to set the parametrisation to for sharing parameters 
#'exists on a DataSHIELD server.
#'@description This server function checks some settings used exchange parameters between 
#'DataSHIELD server exists. It also verifies the data owners and governance have allowed the 
#'sharing of parameters in a specific server.
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

.remove.sharing <- function()
{
  name.struct.exists <- any("name.struct" %in% names(get("settings", pos = 1)))
  if (name.struct.exists)
  {
    sharing <- settings$name.struct
    if(exists(sharing, where = 1))
    {
      remove(sharing,pos = 1)
    }
  }
}

#'@name removeExchangeDataDS
#'@title Remove data used to exchange some parameters from a DataSHIELD server
#'@description This server function deletes the data used in the exchange of the parameters from a DataSHIELD server. The settings and sharing data structure are deleted. 
#'This function is important, to keep temporary data being analysed by other processes. 
#'@return TRUE if the settings and sharing variables have been deleted. Otherwise, FALSE
#'@export
removeExchangeDataDS <- function()
{
  
  sharing <- "no_settings"
  if(exists("settings", where = 1))
  {
    .remove.sharing()
    remove("settings", pos = 1)
  }
  return(!exists(sharing, where = 1) & !exists("settings", where = 1))
}

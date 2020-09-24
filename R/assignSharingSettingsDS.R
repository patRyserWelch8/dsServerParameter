


#'@name assignSharingSettingsDS
#'@title  assign the settings required to share parameters
#'@details The function assigns a variable in the global environment of a DataSHIELD server. A 
#'list of parameters is used in the exchange of parameters. These parameters are to identified names
#'of values stored on the server temporarily during the exchange. Some other values are used to initialise 
#'some matrices rows and columns.
#'@export
assignSharingSettingsDS <- function()
{
    settings <- list( sharing.allowed = 0,
                      name.struct     = "sharing",
                      masking         = "masking",
                      concealing      = "concealing",
                      received        = "received",
                      encrypted       = "encrypted",
                      decrypted       = "decrypted",
                      data            = "data",
                      index_x         = "index_x",
                      index_y         = "index_y",
                      param_names     = "param_names",
                      no_columns      = "no_columns",
                      no_rows         = "no_rows",
                      min_rows        = 11,#11,
                      max_rows        = 21,#21,
                      min_columns     = 13,#13,
                      max_columns     = 23,#23,
                      min_value       = 1)
      
      if (!is.null(getOption("param.name.struct")))
      {
        if(is.character(getOption("param.name.struct")) & 
           !identical(getOption("param.name.struct"), ""))
        {
          settings$name.struct <- getOption("param.name.struct")
        }
      }
    
      if(!is.null(getOption("param.sharing.allowed")))
      {
        if (getOption("param.sharing.allowed") == 0)
        {
          settings$sharing.allowed <- FALSE
        }
        else if (getOption("param.sharing.allowed") == 1)
        {
          settings$sharing.allowed <- TRUE
        }
        else
        {
          settings$sharing.allowed <- FALSE
        }
        
      }
    
      assign("settings",settings,pos=1)
      return(exists("settings",where=1))
}

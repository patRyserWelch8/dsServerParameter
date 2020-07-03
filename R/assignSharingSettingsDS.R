#'@name assignSharingSettingsDS
#'@title  assign the settings required to share parameters
#'@details The function assigns a variable in the global environment of a DataSHIELD server. A 
#'list of parameters is used in the exchange of parameters. These parameters are to identified names
#'of values stored on the server temporarily during the exchange. Some other values are used to initialise 
#'some matrices rows and columns.
#'@export
assignSharingSettingsDS <- function()
{
    settings <- list( name.struct = "sharing",
                      masking     = "masking",
                      concealing  = "concealing",
                      received    = "received",
                      encrypted   = "encrypted",
                      decrypted   = "decrypted",
                      data        = "data",
                      index_x     = "index_x",
                      index_y     = "index_y",
                      param_names = "param_names",
                      no_columns  = "no_columns",
                      no_rows     = "no_rows",
                      min_rows    = 11,#11,
                      max_rows    = 21,#21,
                      min_columns = 13,#13,
                      max_columns = 23,#23,
                      min_value   = 1)
      assign("settings",settings,pos=1)
      return(exists("settings",where=1))
}

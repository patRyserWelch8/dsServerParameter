#'@name setSharingSettingsDS
#'@title  encrypt some data on the server 
#'@export
setSharingSettingsDS <- function()
{
  
  setting <- list(name.struct = "sharing",
                  masking     = "masking",
                  concealing  = "concealing",
                  received    = "received",
                  encrypted   = "encrypted",
                  decrypted   = "decrypted",
                  data        = "data",
                  index_x     = "index_x",
                  index_y     = "index_y",
                  no_columns  = "no_columns",
                  no_rows     = "no_rows",
                  min_rows    = 3,#11,
                  max_rows    = 3,#21,
                  min_columns = 5,#13,
                  max_columns = 5,#23,
                  min_value   = 1)
  assign("settings",setting,pos=1)
  return(exists("settings",where=1))
}

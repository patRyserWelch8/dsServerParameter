#'@name getEncodedDataDS
#'@title  Retrieves the encoded data
#'@description This server function retrieves some encoded data to be passed onto the analysis computer
#'@export

getEncodedDataDS <- function()
{
  return.value <- matrix(rep(0,4),2,2)
  if(exists("sharing"))
  {
    sharing               <- get("sharing",pos = 1)
    encoded.matrix.exists <- "encoded.matrix" %in% names(sharing)

    if(encoded.matrix.exists)
    {
      header       <- ""
      data         <- as.character(paste(sharing$encoded.matrix,sep="",collapse=","))
      size         <- as.integer(object.size(sharing))
      no.columns   <- ncol(sharing$encoded.matrix)
      timestamp    <- as.numeric(Sys.time())
      return.value <- list(header = "FM" , payload = data, property.a = size, property.b = no.columns, property.c = timestamp)
    }
  }
  return(return.value)
}

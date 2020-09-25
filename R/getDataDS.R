
.encode.encrypted.data <- function(master_mode = TRUE)
{
  sharing      <- get(settings$name.struct,pos = 1)
  
  if(!("encrypted" %in% names(sharing)))
  {
    return(encode.data.no.sharing())
  }
  else
  {
    no.columns            <- ncol(sharing[[settings$encrypted]])
    no.rows               <- ncol(sharing[[settings$encrypted]])
    
    #transpose has occured. So need to compare rows with columns settings 
    #and columns with rows settings
    if (no.columns >= settings$min_columns & 
        no.rows    >= settings$min_rows)
    {
      encrypted.data <- as.numeric(sharing[[settings$encrypted]])
      index <- runif(1, min =settings$min_rows, max= settings$max_rows)
      return(encode.data.with.sharing(encrypted.data, no.columns, index))
    }
  }
}

#'@name getDataDS
#'@title  Retrieves the encrypted data from a server to the analysis computer
#'@description This server function retrieves some encrypted data to be passed onto the analysis computer
#'@param master_mode Boolean argument. It indicates the mode of a server is a \strong{master} or a \strong{receiver}. By default, set to TRUE.
#'@return a list made of a header, a payload, and four properties[a-d]
#'@details Some encrypted data are transformed into a suitable format to be transferred from a DataSHIELD server to a DataSHIELD client. The property.a indicates
#'the size of the data at encoding time. Property b is the number of columns and property c a timestamp. property d a random number.
#'@seealso \link[dsServerParameter]{assignDataDS}
#'@export
getDataDS <- function(master_mode = TRUE)
{ 
   if (is.sharing.allowed())
   {
     if(!exists(settings$name.struct,where=1))
     {
       stop("SERVER::ERR::PARAM::003")
     }
     else
     {
       encoded.data <- .encode.encrypted.data(master_mode)
       if(identical(encoded.data$header, "FM2"))
       {
         stop("SERVER::ERR::PARAM::004") 
       }
       else
       {
         return(encoded.data)
       }
     }
   }
   else
   {
     stop("SERVER::ERR::PARAM::001")
   }
}

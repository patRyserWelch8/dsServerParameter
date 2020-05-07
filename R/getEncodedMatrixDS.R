getEncodedMatrixDS <- function()
{
  return.value <- matrix(rep(0,4),2,2)
  if(exists("sharing"))
  {
    list.attributes       <- ls(sharing)
    encoded.matrix.exists <- !identical(grep("encoded.matrix", list.attributes), integer(0))

    if(encoded.matrix.exists)
    {
      return.value <- atrributes(sharing)$encoded.matrix
    }
  }
  return(return.value)
}

#only used for testing ....
setPiDS <- function(newobj = "newobj")
{
   outcome <- list(FALSE, NULL)
   assign(newobj, pi, pos = 1)
   if(exists(newobj,where = 1))
   {
      outcome[[1]] <-  
      new.var     <- get(newobj, pos = 1)
      outcome[2]  <- class(new.var)
   }
   return(outcome)
}

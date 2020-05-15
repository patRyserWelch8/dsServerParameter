
.create_concealed_vector <- function(no.rows =2, min.value = 0, max.value = 1)
{
  set.seed(as.numeric(Sys.time()))

  #set minimum and maximum values - MAY NEED TO BE AN OPTION IN  SERVERS ...
  #MIN               <- runif(1, min=-10^16, max = -1)
  #MAX               <- runif(1, min=1, max = 10^16)

  #1/5/2020 - Changed to accomodate temporarily the parser issues
  MIN               <- 1
  MAX               <- 2000



  no.rows           <- .define_no_rows()
  no.columns        <- .define_no_columns(no.rows)
  concealed.vector  <- .create_concealed_vector(no.rows,MIN,MAX )
  concealing.matrix <- .createMatrixRUnif(no.rows, no.columns, MIN, MAX)

  masking.matrix    <- diag(no.columns)
  while(masking.matrix == diag(no.columns))
  {
    masking.matrix    <- .createMatrixRUnif(no.columns, no.columns, MIN, MAX)
  }

  encoded.matrix    <- .occult(masking.matrix, concealing.matrix, concealed.vector)


  sharing           <- list(master.vector = as.integer(concealed.vector),
                            concealing.matrix = concealing.matrix,
                            masking.matrix = masking.matrix,
                            encoded.matrix = encoded.matrix)
  assign("sharing", sharing, pos = 1)
  return(.is.structure.valid())
}


.create_concealed_vector <- function(no.rows =2, min.value = 0, max.value = 1)
{
  #1/5/2020 - Changed to accomodate temporarily the parser issues
  return(runif(no.rows,min = min.value, max = max.value))
}

.define_no_rows <- function()
{
  no.rows <- 2
  while(no.rows %% 2 == 0 & no.rows < 11)
  {
    no.rows <- as.integer(runif(1, min = 11, max = 21))
  }
  return(no.rows)
}

.define_no_columns <- function(no.rows = 2)
{
  if (is.numeric(no.rows))
  {
      no.columns <- no.rows
      continue = TRUE
      while(continue)
      {
        no.columns <- as.integer(runif(1, min = 13, max = 23))
        continue <- (no.columns %% 2 == 0) | (no.columns == no.rows) | (no.columns < 13)
      }
      return(no.columns)
  }
  else
  {
    stop("incorrect argument")
  }
}

.createMatrixRUnif <- function(no.rows = 11, no.columns = 13, min.value=0, max.value=1)
{
  result <- matrix(c(0),11,13)

  if (is.numeric(no.rows) && is.numeric(no.columns)
      && length(no.rows)  ==  1 && length(no.columns) == 1)
  {

    if (no.rows < 11 || no.columns < 13)
    {
      no.rows <- 11
      no.columns <- 13
    }

    
    random.numbers <- runif(no.rows * no.columns, min = min.value, max = max.value)
    #1/5/2020 - Changed to accomodate temporarily the parser issues - remove ceiling.
    result <- matrix(ceiling(random.numbers),no.rows,no.columns)

  }

  return(result)
}

.occult <- function(masking.matrix = NULL, concealing.matrix = NULL, concealed.vector = NULL)
{
  outcome <- NULL
  #check parameters
  if(is.matrix(masking.matrix) & is.matrix(concealing.matrix) & is.vector(concealed.vector))
  {
    #initialise some variables
    no.row = nrow(concealing.matrix)
    no.col = ncol(masking.matrix)
    outcome <- matrix(rep(0,no.row * no.col),no.row, no.col)

    if (length(concealed.vector) == no.row)
    {
        #hide the concealed vector into a column of the matrix
        column                    <- ceiling(ncol(concealing.matrix)/2)
        concealing.matrix[,column]<- concealed.vector

        #encode the concealing matrix with the masking matrix
        masking.matrix.t    <- t(masking.matrix)
        concealing.matrix.t <- t(concealing.matrix)


        if (ncol(masking.matrix.t) == nrow(concealing.matrix.t))
        {
          outcome <- masking.matrix.t %*% concealing.matrix.t
        }
    }
  }
  return(outcome)
}

.is.structure.valid <- function()
{
  correct <- FALSE
  expected.list <- c("concealing.matrix","encoded.matrix","masking.matrix","master.vector")
  if(exists("sharing"))
  {
    if (is.list(sharing))
    {
      no.elements <- length(sharing)
      if (no.elements == length(expected.list))
      {
          list.attributes <- ls(sharing)
          attributes.exist <- list.attributes == expected.list
          correct <- sum(attributes.exist) == no.elements
      }
    }
  }
  return(correct)
}

.define_no_rows <- function()
{
  no.rows <- 2
  while(no.rows %% 2 == 0)
  {
    no.rows <- as.integer(runif(1, min = 11, max = 21))
  }
  return(no.rows)
}

.define_no_columns <- function(no.rows = 2)
{
   no.columns = 0
   if(is.numeric(no.rows))
   {
      no.columns <- no.rows
      continue = TRUE
      while(continue)
      {
        no.columns <- as.integer(runif(1, min = 13, max = 23))
        continue <- (no.columns %% 2 == 0) | (no.columns == no.rows)
      }
      
   }
   return(no.columns)
   
}

.createMatrixRUnif <- function(no.rows = 11, no.columns = 13, min.value=0, max.value=1)
{
  result <- matrix(c(0),11,13)

  if (is.numeric(no.rows) && is.numeric(no.columns)
      && length(no.rows)  ==  1 && length(no.columns) == 1)
  {
    set.seed(as.numeric(Sys.time()))

    if (no.rows < 11 || no.columns < 13)
    {
      no.rows <- 11
      no.columns <- 13
    }
    #remove ceiling after new parsers
    random.numbers <- as.integer(ceiling(runif(no.rows * no.columns, min = min.value, max = max.value)))
    result <- matrix(random.numbers,no.rows,no.columns)
  }

  return(result)
}

.occult <- function(masking.matrix = NULL, concealing.matrix = NULL, concealed.vector = NULL, master = TRUE)
{
  outcome <- NULL
  #check parameters

  if(is.matrix(masking.matrix) & is.matrix(concealing.matrix) & is.vector(concealed.vector))
  {
    #initialise some variables
    no.row = nrow(concealing.matrix)
    no.col = ncol(masking.matrix)
    outcome <- matrix(rep(0,no.row * no.col),no.row, no.col)
   
    if (nrow(concealing.matrix) == length(concealed.vector))
    {
      #hide the concealed vector into a column of the matrix
      column <- ceiling(ncol(concealing.matrix)/2)
      concealing.matrix[,column]<- concealed.vector
    
      #apply rules master and receiver rules. 
      if (master) 
      { 
          #encode the concealing matrix with the transpose of  masking matrix and concealing matrix
          masking     <- t(masking.matrix)
          concealing  <- t(concealing.matrix)
      }
      else
      {
        #encode the concealing matrix without transporing masking matrix and concealing matrix
        masking     <- masking.matrix
        concealing  <- concealing.matrix
      }
      
      #complete multiplication
      if (ncol(masking) == nrow(concealing))
      {
         outcome <- masking %*% concealing
      }
    }
  }
  return(outcome)
}

.isStructureValid <- function()
{
  correct <- FALSE
  expected.list <- c("concealing.matrix","encoded.matrix","masking.matrix","master.vector")
  if(exists("sharing",where=1))
  {
    no.elements <- length(sharing)
    if (no.elements == length(expected.list))
    {
      list.attributes   <- ls(sharing)
      attributes.exist  <- list.attributes == expected.list
      correct           <- sum(attributes.exist) == no.elements
    }

  }
  return(correct)
}

.create.structure.master <- function(min, max)
{
    no.rows           <- .define_no_rows()  
    no.columns        <- .define_no_columns(no.rows) 
    concealed.vector  <- .create_concealed_vector(no.rows,min, max) 
    concealing.matrix <- .createMatrixRUnif(no.rows, no.columns, min, max) 
    masking.matrix    <- .createMatrixRUnif(no.columns, no.columns, min, max) ######
    encoded.matrix    <- .occult(masking.matrix, concealing.matrix, concealed.vector, TRUE)
    
    outcome           <- list(master.vector = concealed.vector,
                                        concealing.matrix = concealing.matrix,
                                        masking.matrix = masking.matrix,
                                        encoded.matrix = encoded.matrix)
    return(outcome)
}

.create.structure.receiver <- function(min, max)
{
  outcome <- list()
  if(exists("sharing",where=1))
  {
    received.data     <-  get("sharing", pos = 1)
    received.matrix.exists <- "received.matrix" %in% names(sharing)
    if (received.matrix.exists)
    {
      no.rows.received     <- nrow(received.data$received.matrix)
      no.columns.received  <- ncol(received.data$received.matrix)
      concealed.vector     <- .create_concealed_vector(no.columns.received,min, max)
  
      
      #The matrix dimension uses again the dimension of the received matrix. The number of rows for the concealing matrix is the 
      #number of columns from the received.matrix. The number of columns for the concealing matrix is the 
      #number of rows from the received.matrix. 
      
      concealing.matrix <- .createMatrixRUnif(no.columns.received, no.rows.received, min, max) 
     
      #this is different than the master.  We use again the information sent by the master to encode the data.
      masking.matrix    <- received.data$received.matrix 
      encoded.matrix    <- .occult(masking.matrix, concealing.matrix, concealed.vector, FALSE)
    
      outcome           <- list(master.vector = concealed.vector,
                              concealing.matrix = concealing.matrix,
                              masking.matrix = masking.matrix,
                              encoded.matrix = encoded.matrix,
                              received.matrix = received.data$received.matrix)
    }
  }
  return(outcome)
}



#'@name initiateExchangeDS
#'@title  initiates the first steps of a parameter in a master server
#'@description This server function creates the data required to exchange a parameter securely between two
#'DataSHIELD server.
#'@export

initiateExchangeDS <- function(master=TRUE)
{

  #set minimum and maximum values - MAY NEED TO BE AN OPTION IN  SERVERS ...
  #To be changed back to these values
 # MIN             <- runif(1, min=-10^16, max = -1)
#  MAX               <- runif(1, min=1, max = 10^16)
 
  MIN               <- runif(1, min=1, max = 20)
  MAX               <- runif(1, min=30, max =40)

  if (master)
  {
    sharing <- .create.structure.master(MIN, MAX)
  }
  else
  {
    sharing <- .create.structure.receiver(MIN, MAX)
  }
  assign("sharing", sharing, pos = 1)
  return(.isStructureValid())
}


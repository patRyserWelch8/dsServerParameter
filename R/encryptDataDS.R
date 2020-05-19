
.extract.hidden.data <- function(sharing)
{
  outcome       <- sharing
  values.exists <- "concealing.matrix" %in% names(sharing)
  if (values.exists)
  {
     concealing.matrix  <- sharing$concealing.matrix
     column             <- as.integer(runif(1,min = 1, max = ncol(concealing.matrix)-1))
     data               <- concealing.matrix[,column]
     outcome[["data"]] <- data
     outcome[["index"]] <- column
  }
  
  return(outcome)
}

.conceal.hidden.data <- function(sharing)
{
  outcome <- sharing
  values.exists  <- all(c("concealing.matrix","data") %in% names(sharing))
  if(values.exists)
  {
    if(length(sharing$data) == nrow(sharing$concealing.matrix))
    {
      column                               <- as.integer(runif(min = 1, max = ncol(sharing$concealing.matrix)))
      outcome$concealing.matrix[,column]   <- sharing$data
      outcome[["index"]]                   <- column
    }
  }
  
  return(outcome)
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





.encrypt <- function(sharing, master_mode = TRUE)
{
  outcome <- sharing
  values.exists  <- all(c("concealing.matrix","masking.matrix") %in% names(sharing))
  if(values.exists)
  {
    #initialise some variables
    no.row         <- nrow(sharing$concealing.matrix)
    no.col         <- ncol(sharing$masking.matrix)
    matrix.product <- matrix(rep(0,no.row * no.col),no.row, no.col)
    
    if(is.matrix(sharing$concealing.matrix) & is.matrix(sharing$masking.matrix))
    {
      #apply rules master and receiver rules. 
      if (master_mode) 
      { 
        #encode the concealing matrix with the transpose of  masking matrix and concealing matrix
        masking     <- t(sharing$masking.matrix)
        concealing  <- t(sharing$concealing.matrix)
      }
      else
      {
        #encode the concealing matrix without transporing masking matrix and concealing matrix
        masking     <- sharing$masking.matrix
        concealing  <- sharing$concealing.matrix
      }
      
      #complete multiplication
      if (ncol(masking) == nrow(concealing))
      {
        matrix.product <- masking %*% concealing
      }
    }
    
    outcome[["encrypted.matrix"]] <- matrix.product
  }
  
  return(outcome)
}



#This helper function create a concealing.matrix and a master matrix. No data has yet to created.
#The number of rows and columns is defined randomly.
.create.structure.master <- function(min, max)
{
    no.rows           <- .define_no_rows()  
    no.columns        <- .define_no_columns(no.rows) 
  #  concealed.vector  <- .create_concealed_vector(no.rows,min, max) 
    concealing.matrix <- .createMatrixRUnif(no.rows, no.columns, min, max) 
    masking.matrix    <- .createMatrixRUnif(no.columns, no.columns, min, max) ######
   # encrypted.matrix    <- .occult(masking.matrix, concealing.matrix, concealed.vector, TRUE)
    
    outcome           <- list(concealing.matrix = concealing.matrix,
                              masking.matrix = masking.matrix)
    return(outcome)
}

#This helper function creates the concealing matrix. The matrix dimension uses again the dimension of the received matrix. 
#The number of rows for the concealing matrix is the number of columns from the received.matrix. The number of columns 
#for the concealing matrix is the number of rows from the received.matrix.  The masking matrix becomes the received matrix.
.create.structure.receiver <- function(min, max)
{
  outcome <- list()
  if(exists("sharing",where=1))
  {
    received.data          <-  get("sharing", pos = 1)
    received.matrix.exists <- "received.matrix" %in% names(sharing)
    if (received.matrix.exists)
    {
      no.rows.received      <- nrow(received.data$received.matrix)
      no.columns.received   <- ncol(received.data$received.matrix)
     # concealed.vector     <- .create_concealed_vector(no.columns.received,min, max)
      concealing.matrix <- .createMatrixRUnif(no.columns.received, no.rows.received, min, max) 
     
      #this is different than the master.  We use again the information sent by the master to encode the data.
      masking.matrix    <- received.data$received.matrix 
     # encoded.matrix    <- .occult(masking.matrix, concealing.matrix, concealed.vector, FALSE)
    
      outcome           <- list(concealing.matrix = concealing.matrix,
                                masking.matrix = masking.matrix,
                                received.matrix = received.data$received.matrix)
    }
  }
  return(outcome)
}


.is.structure.valid <- function()
{
  correct <- FALSE
  expected.list <- c("concealing.matrix","encrypted.matrix","masking.matrix","data","index")
 
  if(exists("sharing",where=1))
  {
   
    sharing <- get("sharing", pos=1)
    if (is.list(sharing))
    {
      no.elements <- length(sharing)
      if (no.elements == length(expected.list))
      {
        list.attributes <- names(sharing)
        attributes.exist <- list.attributes %in% expected.list
        correct <- all(list.attributes %in% expected.list)
      }
    }
  }
  return(correct)
}


#'@name encryptDataDS
#'@title  encrypt some data on the server 
#'@description This server function uses some matrices operations to encrypts some data required to exchange a parameter securely between two
#'DataSHIELD server.
#'@param master_mode Boolean argument. It indicates the mode of the encryption. By default, set to TRUE.
#'@param blank_mode  Boolean argument. It indicates the mode of data used for encryption. By default, set to TRUE.
#'@export

encryptDataDS <- function(master_mode=TRUE, blank_mode = TRUE)
{

  #set minimum and maximum values - MAY NEED TO BE AN OPTION IN  SERVERS ...
  #To be changed back to these values
 # MIN             <- runif(1, min=-10^16, max = -1)
#  MAX               <- runif(1, min=1, max = 10^16)
 
  MIN               <- runif(1, min=1, max = 20)
  MAX               <- runif(1, min=30, max =40)

  #create matrices for encryption
  if (master_mode)
  {
    sharing <- .create.structure.master(MIN, MAX)
  }
  else
  {
    sharing <- .create.structure.receiver(MIN, MAX)
  }
  
  #extract or conceals data in concealing matrix
  if(blank_mode)
  {

    sharing <- .extract.hidden.data(sharing) 
  }
  else
  {
    sharing <- .conceal.hidden.data(sharing)
  }
  
  sharing <- .encrypt(sharing, master_mode)
  
  assign("sharing", sharing, pos = 1)
  return(.is.structure.valid())
}


.get_sharing <- function()
{
  outcome <- list()

  if(exists(settings$name.struct, where =1))
  {
    outcome <- get(settings$name.struct, pos=1) 
  }
  return(outcome)
}

.extract.hidden.data <- function(sharing)
{
  data          <- NULL
  value.exists <- all(c(settings$concealing, settings$index_x) %in% names(sharing))
  if (value.exists)
  {
     if(sharing[[settings$index_x]] <= ncol(sharing[[settings$concealing]]))
     {
        data  <- sharing[[settings$concealing]][,sharing[[settings$index_x]]]
     }
  }
  return(data)
}

.conceal.data <- function(sharing, data)
{
  outcome <- sharing
  value.exists  <- settings$concealing %in% names(sharing)
  if(value.exists)
  {
    if(length(data) == nrow(sharing[[settings$concealing]]))
    {
      column                                    <- as.integer(runif(min = 1, max = ncol(sharing$concealing)))
      outcome[[settings$concealing]][,column]   <- data
      outcome[[settings$index_x]]               <- column
    }
  }
  
  return(outcome)
}


.define_no_rows <- function()
{
  no.rows <- 2
  while(no.rows %% 2 == 0 & no.rows < settings$min_rows)
  {
    no.rows <- as.integer(runif(1, min = settings$min_rows, max = settings$max_rows))
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
        no.columns <- as.integer(runif(1, min = settings$min_columns, max = settings$max_columns))
        continue <- (no.columns %% 2 == 0) | (no.columns == no.rows) | (no.columns < 13)
      }
      return(no.columns)
  }
  else
  {
    stop("incorrect argument")
  }
}

.createMatrixRUnif <- function(no.rows = settings$min_rows, no.columns = settings$min_columns, min.value=0, max.value=1)
{
  result <- matrix(c(0),settings$min_rows,settings$min_columns)

  if (is.numeric(no.rows) && is.numeric(no.columns)
      && length(no.rows)  ==  1 && length(no.columns) == 1)
  {

    if (no.rows < settings$min_rows || no.columns < settings$min_columns)
    {
      no.rows    <- settings$min_rows
      no.columns <- settings$min_columns
    }
    
    random.numbers <- runif(no.rows * no.columns, min = min.value, max = max.value)
    #1/5/2020 - Changed to accomodate temporarily the parser issues - remove ceiling.
    result <- matrix(ceiling(random.numbers),no.rows,no.columns)
  }

  return(result)
}


.encrypt <- function(sharing, master_mode = TRUE)
{
  outcome <- sharing
  values.exists  <- all(c(settings$concealing,settings$masking) %in% names(sharing))
  if(values.exists)
  {
    #initialise some variables
    no.row         <- nrow(sharing[[settings$concealing]])
    no.col         <- ncol(sharing[[settings$masking]])
    matrix.product <- matrix(rep(0,no.row * no.col),no.row, no.col)
    
    if(is.matrix(sharing[[settings$concealing]]) & is.matrix(sharing[[settings$masking]]))
    {
      #apply rules master and receiver rules. 
      if (master_mode) 
      { 
        #encode the concealing matrix with the transpose of  masking matrix and concealing matrix
        masking     <- t(sharing[[settings$masking]])
        concealing  <- t(sharing[[settings$concealing]])
      }
      else
      {
        #encode the concealing matrix without transporing masking matrix and concealing matrix
        masking     <- sharing[[settings$masking]]
        concealing  <- sharing[[settings$concealing]]
      }
      
      #complete multiplication
      if (ncol(masking) == nrow(concealing))
      {
        matrix.product <- masking %*% concealing
      }
    }
    
    outcome[[settings$encrypted]] <- matrix.product
  }
  
  return(outcome)
}

#This helper function create a concealing.matrix and a master matrix. No data has yet to created.
#The number of rows and columns is defined randomly.
.create.structure.master <- function(min, max)
{
    outcome                         <- list()
    no.rows                         <- .define_no_rows()  
    no.columns                      <- .define_no_columns(no.rows) 
    outcome[[settings$concealing]]  <- .createMatrixRUnif(no.rows, no.columns, min, max) 
    outcome[[settings$masking]]     <- .createMatrixRUnif(no.columns, no.columns, min, max) 
    return(outcome)
}

#This helper function creates the concealing matrix. The matrix dimension uses again the dimension of the received matrix. 
#The number of rows for the concealing matrix is the number of columns from the received.matrix. The number of columns 
#for the concealing matrix is the number of rows from the received.matrix.  The masking matrix becomes the received matrix.
.create.structure.receiver <- function(min, max)
{
  outcome <- list()
  if(exists(settings$name.struct,where=1))
  {
    received.data    <-  get(settings$name.struct, pos = 1)
    value.exists     <- settings$received %in% names(sharing)
    if (value.exists)
    {
      no.rows.received               <- nrow(received.data[[settings$received]])
      no.columns.received            <- ncol(received.data[[settings$received]])
      outcome                        <- list()
      outcome[[settings$concealing]] <- .createMatrixRUnif(no.columns.received, no.rows.received, min, max) 
     
      #this is different than the master.  We use again the information sent by the master to encode the data.
      outcome[[settings$masking]]    <- received.data[[settings$received]] 
    }
  }
  return(outcome)
}


.is.encrypted.valid <- function(sharing, expected.fields)
{
  correct <- FALSE
 
  if (is.list(sharing) & is.vector(expected.fields))
  {
    list.attributes <- names(sharing)
    attributes.exist <- list.attributes %in% expected.fields
    total.correct = sum(attributes.exist == TRUE)
    correct <- total.correct == length(expected.fields)
   }
 
  return(correct)
}


#'@name encryptDataDS
#'@title  encrypt some data on the server 
#'@description This server function uses some matrices operations to encrypts some data required to exchange a parameter securely between two
#'DataSHIELD server.
#'@param master_mode Boolean argument. It indicates the mode of the encryption. By default, set to TRUE.
#'@param preserve_mode  Boolean argument. It indicates to presever some data exchanged previously between servers. By default, set to FALSE.
#'@export

encryptDataDS <- function(master_mode=TRUE, preserve_mode = FALSE)
{
  outcome <- FALSE
  if (exists("settings", where = 1))
  {
      #set minimum and maximum values - MAY NEED TO BE AN OPTION IN  SERVERS ...
      #To be changed back to these values
     # MIN             <- runif(1, min=-10^16, max = -1)
    #  MAX  <- runif(1, min=1, max = 10^16)
      
      #init variables
      MIN     <- runif(1, min=settings$min_value, max  = settings$min_value + 20)
      MAX     <- runif(1, min=settings$min_value+30, max = settings$min_value + 40)
      data    <- NULL
      sharing <- .get_sharing()
      
      #preserve the data from previous exchange
      if (preserve_mode)
      {
          data <- .extract.hidden.data(sharing)
      }
      
      #create matrices for encryption
      if (master_mode)
      {
          sharing <- .create.structure.master(MIN, MAX)
      }
      else
      {
         sharing <- .create.structure.receiver(MIN, MAX)
      }
        
      if (preserve_mode)
      {
         sharing <- .conceal.data(sharing,data)
      }
        
      sharing       <- .encrypt(sharing, master_mode)
      expected.list <- c(settings$masking,settings$encrypted)
      sharing       <- sharing[names(sharing) %in% expected.list == TRUE]
      assign(settings$name.struct, sharing, pos = 1)
      outcome <- .is.encrypted.valid(sharing, expected.list) & 
                 exists(settings$name.struct, where=1)
  }
  return(outcome)
}




.get_sharing <- function()
{
  outcome <- list()

  if(exists(settings$name.struct, where =1))
  {
    outcome <- get(settings$name.struct, pos=1) 
    print("^^^^^^")
    print(outcome$index_y)
  }
  return(outcome)
}


.conceal.data <- function(sharing, data)
{
  outcome               <- sharing 
  expected.data         <- c(settings$data, settings$index_x, settings$index_y, settings$no_columns, settings$no_rows)
  value.exists.sharing  <- settings$concealing %in% names(sharing)
  value.exists.data     <- all(expected.data %in% names(data))
  if(value.exists.sharing & value.exists.data)
  {
    print("I am in again")
    print(length(data[[settings$data]]))
    print(nrow(sharing[[settings$concealing]]))
    print(length(data[[settings$data]]) == nrow(sharing[[settings$concealing]]))
    if(length(data[[settings$data]]) == nrow(sharing[[settings$concealing]]))
    {
      
      outcome[[settings$concealing]][,data[[settings$index_x]] ] <- data[[settings$data]]
      outcome[[settings$index_x]]                                   <- data[[settings$index_x]]
      outcome[[settings$index_y]]                                   <- data[[settings$index_y]]
      outcome[[settings$no_columns]]                                <- data[[settings$no_columns]]
      outcome[[settings$no_rows]]                                   <- data[[settings$no_rows]]
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
  names(outcome)
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
   names(outcome)
  return(outcome)
}

#This helper function create a concealing.matrix and a master matrix. No data has yet to created.
#The number of rows and columns is defined randomly. The index_y is set. When encrypted the matrix is transposed and 
#the chosen column becomes a row. 
.create.structure.master <- function(min, max, no.rows = 0, no.columns = 0)
{
    outcome                         <- list()
    
    if (no.rows == 0 & no.columns == 0)
    {
      no.rows                         <- .define_no_rows()  
      no.columns                      <- .define_no_columns(no.rows) 
      outcome[[settings$index_y]]     <-  ceiling(runif(1, min = 0, max = no.columns-1))
    }
    outcome[[settings$concealing]]  <- .createMatrixRUnif(no.rows, no.columns, min, max) 
    outcome[[settings$masking]]     <- .createMatrixRUnif(no.columns, no.columns, min, max) 
    outcome[[settings$no_columns]]  <- no.columns
    outcome[[settings$no_rows]]     <- no.rows
    
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
      outcome[[settings$index_x]]    <-  ceiling(runif(1, min = 0, max = no.columns.received-1))
      outcome[[settings$data]]       <- outcome[[settings$concealing]][,outcome[[settings$index_x]]]
      outcome[[settings$index_y]]    <- received.data[[settings$index_y]]
      #this is different than the master.  We use again the information sent by the master to encode the data.
      outcome[[settings$masking]]     <- received.data[[settings$received]]
      outcome[[settings$no_columns]]  <- no.columns.received
      outcome[[settings$no_rows]]     <- no.rows.received
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
      MIN            <- runif(1, min=settings$min_value, max  = settings$min_value + 20)
      MAX            <- runif(1, min=settings$min_value+30, max = settings$min_value + 40)
      data           <- NULL
      expected.list  <- c()
      preserved.data <- c()
      no.columns     <- 0
      no.rows        <- 0 
      sharing        <- .get_sharing()
      
      #preserve the data from previous exchange
      if (preserve_mode)
      {
         preserved.data[[settings$data]]       <- sharing[[settings$data]]
         preserved.data[[settings$index_x]]    <- sharing[[settings$index_x]]
         preserved.data[[settings$index_y]]    <- sharing[[settings$index_y]]
         preserved.data[[settings$no_columns]] <- sharing[[settings$no_columns]]
         preserved.data[[settings$no_rows]]    <- sharing[[settings$no_rows]]
         #transpose the number rows and columns. The length of the data matches the master. 
         no.columns                            <- sharing[[settings$no_rows]]
         no.rows                               <- sharing[[settings$no_columns]]
      }
      
      #create matrices for encryption
      if (master_mode)
      {
          sharing <- .create.structure.master(MIN, MAX, 
                                              no.rows = no.rows, 
                                              no.columns = no.columns)
          
          expected.list <- c(settings$masking,settings$encrypted, settings$index_y,settings$no_columns, settings$no_rows)
      }
      else
      {
         sharing <- .create.structure.receiver(MIN, MAX)
         expected.list <- c(settings$masking,settings$encrypted, settings$index_x,settings$index_y, 
                            settings$data,settings$no_columns, settings$no_rows)
      }
        
      if (preserve_mode)
      {
         sharing       <- .conceal.data(sharing,preserved.data)
         expected.list <- c(settings$masking,settings$encrypted, settings$index_x,settings$index_y, 
                            settings$no_columns, settings$no_rows)
      }
  
      sharing       <- .encrypt(sharing, master_mode)
      sharing       <- sharing[names(sharing) %in% expected.list == TRUE]
      print(names(sharing))
     
      assign(settings$name.struct, sharing, pos = 1)
      outcome       <- .is.encrypted.valid(sharing, expected.list) & 
                       exists(settings$name.struct, where=1)
  }
  return(outcome)
}




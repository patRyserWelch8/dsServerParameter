.get_sharing <- function()
{
  outcome <- list()

  if(exists(settings$name.struct, where =1))
  {
    outcome <- get(settings$name.struct, pos=1) 
   
  }
  return(outcome)
}

.conceal.data <- function(concealing.matrix, data, columns)
{
 
  if(is.matrix(concealing.matrix) & is.list(data) & is.vector(columns))
  {
  
    no.params <- length(data)
   
    if (length(data) == length(columns))
    {
      
      outcome   <- concealing.matrix
      
      for (index in 1:no.params)
      {
          column <- columns[index]
          if(length(data[[index]]) == nrow(concealing.matrix) & 
             column < ncol(concealing.matrix))
          {
             concealing.matrix[,column]  <- data[[index]]
          }
      }
    }
  }
  return(concealing.matrix)
}


.define_no_rows <- function()
{
  no.rows <- 2
  continue <- TRUE
  while(continue)
  {
    no.rows <- as.integer(runif(1, min = settings$min_rows, max = settings$max_rows))
    if(no.rows %% 2 == 1  & 
       no.rows >= settings$min_columns & no.rows <= settings$max_columns) 
    {
      continue = FALSE
    }
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
        if(no.columns %% 2 == 1 &  no.columns != no.rows  & 
           no.columns >= settings$min_columns & no.columns<= settings$max_columns) 
        {
           continue = FALSE
        }
        
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
    result         <-  matrix(random.numbers,no.rows,no.columns)
    
  }

  return(result)
}


.encrypt.concealed.data <- function(sharing, master_mode = TRUE)
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
#The number of rows and columns is defined randomly. The index_y is set. When encrypted the matrix is transposed and 
#the chosen column becomes a row. 
.create.structure.master <- function(min, max,no.rows, no.columns)
{
  
    outcome                         <- list()
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
      #transpose is required counter the transpose in master
      no.rows.received               <- nrow(t(received.data[[settings$received]]))
      no.columns.received            <- ncol(t(received.data[[settings$received]]))
      
      outcome[[settings$concealing]] <- .createMatrixRUnif(no.rows.received, no.columns.received, min, max) 
    
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

.preserve.info <- function(sharing)
{
 
  outcome      <- list()
  field_names  <- names(sharing)
  correct_info <- all(c(settings$index_x,settings$index_y) 
                      %in% field_names)
  data_exists <- all(settings$data %in% field_names)
  
  if(correct_info)
  {
       if (all(sharing[[settings$index_x]] < 1) &
           all(sharing[[settings$index_y]] < 1))
       {
          outcome[[settings$index_x]] <- sharing[[settings$index_x]]
          outcome[[settings$index_y]] <- sharing[[settings$index_y]]  
          outcome[["columns"]]        <- ceiling(sharing[[settings$index_x]] * sharing[[settings$no_columns]])
          outcome[["rows"]]           <- ceiling(sharing[[settings$index_y]] * sharing[[settings$no_rows]])
       
          if(data_exists)
          {
              #extract the data save earlier in the process 
              outcome[[settings$data]] <- sharing[[settings$data]]
          }
          else
          {
         
              #extract each column to be preserved from the concealing matrix 
              outcome[[settings$data]] <- list()
        
              for(index in 1:length(outcome[["columns"]]))
              {
                column <- outcome[["columns"]][index]
                outcome[[settings$data]][[index]] <- sharing[[settings$concealing]][,column]
              }
          }
       }
  }
 
 
  return(outcome)
}

.encrypt.data <- function(master_mode, preserve_mode)
{
  #init variables
  MIN            <- runif(1, min=settings$min_value, max  = settings$min_value + 20)
  MAX            <- runif(1, min=settings$min_value+30, max = settings$min_value + 40)
  data           <- NULL
  
  expected.list  <- c()
  no.rows        <- .define_no_rows()  
  no.columns     <- .define_no_columns(no.rows)   
  sharing        <- .get_sharing()
  saved.info     <- list()  
  
  #preserve the data from previous exchange, if it is required.
  if(preserve_mode) #steps 6 and 8
  {
    saved.info    <- .preserve.info(sharing) 
    no.rows       <-  sharing[[settings$no_rows]]
    no.columns    <-  sharing[[settings$no_columns]]
  }
  
  #create matrices for encryption. 
 
  if (master_mode)
  {
    #master_mode is in steps 1 and 6 of the exchange 
    sharing <- .create.structure.master(MIN, MAX, 
                                        no.rows = no.rows, 
                                        no.columns = no.columns)
    expected.list <- c(settings$concealing,settings$masking,
                       settings$encrypted,settings$no_columns, settings$no_rows)
  }
  else
  {
    # not master mode: steps 3 and 8
    sharing <- .create.structure.receiver(MIN, MAX)
    expected.list <- c(settings$concealing,settings$masking,settings$encrypted,settings$no_columns, settings$no_rows)
  }
  
  if (preserve_mode) #steps 6 and 8
  {
    if(master_mode) #step 6 
    {
      
      sharing[[settings$concealing]] <- .conceal.data(sharing[[settings$concealing]],
                                                      saved.info[[settings$data]],saved.info[["rows"]])
    }
    else #step 8
    {
      sharing[[settings$concealing]] <- .conceal.data(sharing[[settings$concealing]],
                                                      saved.info[[settings$data]],saved.info[["columns"]])
    }
    sharing[[settings$index_x]]    <-  saved.info[[settings$index_x]]
    sharing[[settings$index_y]]    <-  saved.info[[settings$index_y]]
    
    expected.list <- c(settings$concealing,settings$masking,settings$encrypted,
                       settings$no_columns, settings$no_rows, settings$index_x, settings$index_y)
  }
  
  
  sharing     <- .encrypt.concealed.data(sharing, master_mode)
  assign(settings$name.struct, sharing, pos = 1)
  return(expected.list)
  
}

#'@name encryptDataDS
#'@title  encrypt some data on the server 
#'@description This server function uses some matrices operations to encrypts some data required to exchange a parameter securely between two DataSHIELD server.
#'@details This function encrypts data in four different ways; each of them are defined by the combination of argument values.
#'\itemize{
#'\item{\code{master_mode = TRUE, preserve_mode = FALSE}} {- This combination of argument values indicates the server is in \strong{master_mode}. An exchange of encrypted data is  initiated. It is the first step of the exchange.}  
#'\item{\code{master_mode = FALSE, preserve_mode = FALSE}} {- This set of argument values states the server is in \strong{receiver_mode}. Some data received from the master is 
#'used again to encrypted a \strong{receiver_server specific} data. }
#'\item{\code{master_mode = TRUE, preserve_mode = TRUE}} {- When both argument values are set to true, some data used previously in the process are 
#'concealed in within some newly generated data.}
#'\item{\code{master_mode = FALSE, preserve_mode = TRUE}} {- This set of argument values states the server is in \strong{receiver_mode}. Some data received from the master is 
#'used again to encrypted a \strong{receiver_server specific} data. Some data previously used by the process are preserved.}
#'}
#'@param master_mode Boolean argument. It indicates the mode of encryption. By default, set to TRUE.
#'@param preserve_mode  Boolean argument. It indicates to presever some data exchanged previously between servers. By default, set to FALSE.
#'@export
encryptDataDS <- function(master_mode = TRUE, preserve_mode = FALSE)
{
  if (is.logical(master_mode) & is.logical(preserve_mode))
  {
     
     if(is.sharing.allowed())
     {
       expected.list <- .encrypt.data(master_mode, preserve_mode)   
       outcome       <- .is.encrypted.valid(sharing, expected.list) & 
                        exists(settings$name.struct, where=1)
       return(outcome)
     }
     else
     {
       stop("SERVER::ERR::PARAM::001")
     }
  }
  else
  {
    stop("SERVER::ERR::PARAM::002")
  }
}




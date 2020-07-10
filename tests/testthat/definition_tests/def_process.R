init.process <- function()
{
    assign("param", 1000, pos=1)
    assignSharingSettingsDS()
}


create.init.matrices.master <- function ()
{
    if(exists("sharing.master",where=1))
    {
         rm("sharing.master", pos =1)
    }
    
    if(exists("sharing.receiver",where=1))
    {
        rm("sharing.receiver", pos =1)
    }
    
    if(exists("sharing",where=1))
    {
        rm("sharing", pos =1)
    }
   
    #step 1 - master 
    success <- encryptDataDS(master_mode = TRUE, preserve_mode = TRUE)
    sharing.master <- get("sharing",pos =1)
    assign("sharing.master", sharing.master, pos=1)
}

transfer.matrices.from.master.to.receiver <- function ()
{
    #step 2 - transfer 
    data    <- getEncodedDataDS()
  
    rm("sharing", pos=1)
   
    result  <- sendEncodedDataDS(data$header, data$payload, data$property.a, data$property.b, data$property.c, data$property.d)
    sharing.receiver <- get("sharing",pos =1)
    assign("sharing.receiver", sharing.receiver, pos=1)
   
    
}

create.init.matrices.receiver <- function ()
{
    #step 3 - receiver 
    success <- initiateExchangeDS(master = FALSE)
    sharing.receiver <- get("sharing",pos =1)
    assign("sharing.receiver", sharing.receiver, pos=1)
   
}

transfer.matrices.from.receiver.to.master <- function()
{
    #step 4 - transfer 
    data    <- getEncodedDataDS()
   
    rm("sharing", pos=1)
    assign("sharing", sharing.master, pos=1)
    result  <- sendEncodedDataDS(data$header, data$payload, data$property.a, data$property.b, data$property.c, data$property.d)
    sharing.master <- get("sharing",pos =1)
    assign("sharing.master", sharing.master, pos=1)
   
}

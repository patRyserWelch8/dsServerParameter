


do.step.1 <- function ()
{
    rm("sharing.master", pos =1)
    rm("sharing.receiver", pos =1 )
    rm("sharing", pos=1)
    #step 1 - master 
    success <- initiateExchangeDS(master = TRUE)
    sharing.master <- get("sharing",pos =1)
    assign("sharing.master", sharing.master, pos=1)
    print("step 1 - sharing.master")
    print(dim(sharing.master$encoded.matrix))
}

do.step.2 <- function ()
{
    #step 2 - transfer 
    data    <- getEncodedDataDS()
    print(data$header)
    rm("sharing", pos=1)
    print(ls(pos=1))
    result  <- sendEncodedDataDS(data$header, data$payload, data$property.a, data$property.b, data$property.c, data$property.d)
    sharing.receiver <- get("sharing",pos =1)
    assign("sharing.receiver", sharing.receiver, pos=1)
    print("step 2- sharing.receiver")
    print(dim(sharing.receiver$received.matrix))
    
}

do.step.3 <- function ()
{
    #step 3 - receiver 
    success <- initiateExchangeDS(master = FALSE)
    sharing.receiver <- get("sharing",pos =1)
    assign("sharing.receiver", sharing.receiver, pos=1)
    print("step 3- sharing.receiver created ")
    print(dim(sharing.receiver$concealing.matrix))
    print(dim(sharing.receiver$encoded.matrix))
}

do.step.4 <- function()
{
    #step 4 - transfer 
    data    <- getEncodedDataDS()
    print(data$header)
    rm("sharing", pos=1)
    assign("sharing", sharing.master, pos=1)
    result  <- sendEncodedDataDS(data$header, data$payload, data$property.a, data$property.b, data$property.c, data$property.d)
    sharing.master <- get("sharing",pos =1)
    assign("sharing.master", sharing.master, pos=1)
    print("step 4- sharing.master")
    
}

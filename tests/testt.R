rm(list=ls())
print("Step 0")
pi_value = 1000
setSharingSettingsDS()

print("Step 1")
encryptDataDS(TRUE, FALSE)
master.1 <- get("sharing",pos=1)

print("Step 2")
a <- getDataDS(master_mode =TRUE)
rm(sharing,pos=1)
assignDataDS(master_mode = FALSE,a$header,a$payload,a$property.a,a$property.b,a$property.c,a$property.d)
receiver.1 <- get("sharing",pos=1)

print("Step 3")
encryptDataDS(FALSE, FALSE)
receiver.2 <- get("sharing",pos=1)

print("step 4")
b <- getDataDS(master_mode =  FALSE)
rm(sharing,pos=1)
assign("sharing", master.1, pos=1)
assignDataDS(master_mode = TRUE, b$header,b$payload,b$property.a,b$property.b,b$property.c,b$property.d)
master.2 <- get("sharing",pos=1)


print("step 5")
decryptDataDS()
master.3 <- get("sharing",pos=1)
encryptParamDS("pi_value")
master.4 <- get("sharing",pos=1)
removeEncryptingDataDS()
master.5 <- get("sharing",pos=1)

print("step 6 - Receiver becomes master .... ")
assign("sharing", receiver.2, pos=1)
removeEncryptingDataDS()
receiver.3 <- get("sharing",pos=1)
encryptDataDS(TRUE, TRUE)
receiver.4 <- get("sharing",pos=1)

print("step 7")
c <- getDataDS(master_mode = TRUE)
rm(sharing,pos=1)
assign("sharing", master.5, pos=1)
assignDataDS(master_mode = FALSE,c$header,c$payload,c$property.a,c$property.b,c$property.c,c$property.d)
master.6 <- get("sharing",pos=1)


print("step 8 ")
encryptDataDS(FALSE, TRUE)
master.7 <- get("sharing",pos=1)



print("step 9")
d <- getDataDS(master_mode = FALSE)
rm(sharing,pos=1)
assign("sharing", receiver.4, pos=1)
print(d$payload)
assignDataDS(master_mode = TRUE,d$header,d$payload,d$property.a,d$property.b,d$property.c,d$property.d)
receiver.5 <- get("sharing",pos=1)


print("step 10")
decryptDataDS() 
receiver.6 <- get("sharing",pos=1)


# This file simulates the exchanges of parameters using a range of master and 
# receiver variables. It is a good script to execute outside test_that to validate 
# the code works.

library(dsServerParameter)
rm(list=ls(),pos=1)

options(param.name.struct = "sharing")
options(param.sharing.allowed = 1) 

print("Step 0")
pi_value_1 = 100000
pi_value_2 = 200000
pi_value_3 = 300000
assignSharingSettingsDS()


print("Step 1")
encryptDataDS(TRUE, FALSE)
master.1 <- get("sharing",pos=1)

print("Step 2")
a <- getDataDS(master_mode = TRUE)
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
print(names(master.2))

print("step 5")
decryptDataDS()
master.3 <- get("sharing",pos=1)
print(names(master.3))
assignParamSettingsDS(c("pi_value_1","pi_value_2","pi_value_3")) #done
master.3.5 <- get("sharing",pos=1)
print(names(master.3.5))

f<- getCoordinatesDS() #done
rm(sharing,pos=1)
assign("sharing", receiver.2, pos=1)
assignCoordinatesDS(f$header, f$payload,f$property.a,f$property.b,f$property.c,f$property.d)#done
receiver.2.5 <- get("sharing",pos=1)
print(names(receiver.2.5))
rm(sharing,pos=1)
assign("sharing", master.3.5, pos=1)

encryptParamDS()#done

master.4 <- get("sharing",pos=1)
print(names(master.4))
print(master.4$concealing)
print(master.4$encrypted)
removeEncryptingDataDS(master_mode = TRUE) #done
master.5 <- get("sharing",pos=1)
print(names(master.5))

print("step 6 - Receiver becomes master .... ")
assign("sharing", receiver.2.5, pos=1)
removeEncryptingDataDS(master_mode = FALSE)
receiver.3 <- get("sharing",pos=1)
print(names(receiver.3))
encryptDataDS(TRUE, TRUE) 
receiver.4 <- get("sharing",pos=1)
print(names(receiver.4))

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
assignDataDS(master_mode = TRUE,d$header,d$payload,d$property.a,d$property.b,d$property.c,d$property.d)
receiver.5 <- get("sharing",pos=1)


print("step 10")
decryptDataDS()
receiver.6 <- get("sharing",pos=1)
decryptParamDS("pi_value_1_a;pi_value_2_a;pi_value_3_a", 1)

print(pi_value_1_a)
print(pi_value_2_a)
print(pi_value_3_a)

print("columns")
print(ceiling(receiver.6$index_x * receiver.6$no_columns))
print("rows")
print(ceiling(receiver.6$index_y * receiver.6$no_rows))
print("decrypted")
print(receiver.6$decrypted)

outcome <- removeExchangeDataDS()
print(outcome)




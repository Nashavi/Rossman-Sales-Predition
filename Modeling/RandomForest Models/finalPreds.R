
load("Modeling/RandomForest Models/ST_RFModel.RData") #Load the model for a state
load("Datasets/FinalTest.RData") #load the test dataset 
load("Modeling/finalPreds.RData") #load the consolidated prediction file  

st <- "ST"  # assign the state your are applying predictions for 

finalPreds[finalPreds$State==st,"Sales"] #check to see if there are sales values predicted already - should be 0's....


NewSales <- predict(rfFit,newdata = t) #create a vector of predictions 

#conditionally update the sales figure 
#if the state matches, match the newsales figure, otherwise keep the value from the dataset. 
finalPreds$Sales <- ifelse(finalPreds$Open==1 & finalPreds$State==st,NewSales,finalPreds$Sales)

#now save the dataframe off...
save(finalPreds,file="Modeling/finalPreds.RData")






### this is all old stuff from the first model I applied to the finalPreds dataframe. 

############## IGNORE BELOW ##################

# X <- t$X
# Open <- t$Open
# State <- t$State
# Store <- t$Store

# 
# finalPreds <- as.data.frame(cbind(Sales,Store))
# finalPreds <- cbind(State,finalPreds)
# finalPreds <- cbind(Open,finalPreds)
# finalPreds <- cbind(X,finalPreds)


################### FIXES FOR ST MODEL ####################
# t$events.22 <- 0
# t$Open_L1.0 <- ifelse(t$Open_L1.2==1,1,0)
# t$promovalid._L10 <- ifelse(t$promovalid._L11==1,1,0)
# t$promovalid._L20 <- ifelse(t$promovalid._L21==1,1,0)
# 
# t$StateHoliday._L10 <- ifelse(t$StateHoliday._L11==1,1,0)
# t$StateHoliday._L1a <- ifelse(t$StateHoliday._L12==1,1,0)
# t$StateHoliday._L1b <- ifelse(t$StateHoliday._L12==1,1,0)
# t$StateHoliday._L1c <- ifelse(t$StateHoliday._L12==1,1,0)
# 
# t$StateHoliday._L20 <- ifelse(t$StateHoliday._L21==1,1,0)
# t$StateHoliday._L2a <- ifelse(t$StateHoliday._L22==1,1,0)
# t$StateHoliday._L2b <- ifelse(t$StateHoliday._L22==1,1,0)
# t$StateHoliday._L2c <- ifelse(t$StateHoliday._L22==1,1,0)
# 
# t$StateHoliday._L30 <- ifelse(t$StateHoliday._L31==1,1,0)
# t$StateHoliday._L3a <- ifelse(t$StateHoliday._L32==1,1,0)
# t$StateHoliday._L3b <- ifelse(t$StateHoliday._L32==1,1,0)
# t$StateHoliday._L3c <- ifelse(t$StateHoliday._L32==1,1,0)
# 
# t$StateHoliday._L40 <- ifelse(t$StateHoliday._L41==1,1,0)
# t$StateHoliday._L4a <- ifelse(t$StateHoliday._L42==1,1,0)
# t$StateHoliday._L4b <- ifelse(t$StateHoliday._L42==1,1,0)
# t$StateHoliday._L4c <- ifelse(t$StateHoliday._L42==1,1,0)
# 
# t$StateHoliday._L50 <- ifelse(t$StateHoliday._L51==1,1,0)
# t$StateHoliday._L5a <- ifelse(t$StateHoliday._L52==1,1,0)
# t$StateHoliday._L5b <- ifelse(t$StateHoliday._L52==1,1,0)
# t$StateHoliday._L5c <- ifelse(t$StateHoliday._L52==1,1,0)
# 
# 
# t$SchoolHoliday._L10 <- ifelse(t$SchoolHoliday._L11==1,1,0)
# t$SchoolHoliday._L1a <- ifelse(t$SchoolHoliday._L12==1,1,0)
# t$SchoolHoliday._L1b <- ifelse(t$SchoolHoliday._L12==1,1,0)
# t$SchoolHoliday._L1c <- ifelse(t$SchoolHoliday._L12==1,1,0)
# 
# t$SchoolHoliday._L20 <- ifelse(t$SchoolHoliday._L21==1,1,0)
# t$SchoolHoliday._L2a <- ifelse(t$SchoolHoliday._L22==1,1,0)
# t$SchoolHoliday._L2b <- ifelse(t$SchoolHoliday._L22==1,1,0)
# t$SchoolHoliday._L2c <- ifelse(t$SchoolHoliday._L22==1,1,0)
# 
# t$SchoolHoliday._L30 <- ifelse(t$SchoolHoliday._L31==1,1,0)
# t$SchoolHoliday._L3a <- ifelse(t$SchoolHoliday._L32==1,1,0)
# t$SchoolHoliday._L3b <- ifelse(t$SchoolHoliday._L32==1,1,0)
# t$SchoolHoliday._L3c <- ifelse(t$SchoolHoliday._L32==1,1,0)
# 
# t$SchoolHoliday._L40 <- ifelse(t$SchoolHoliday._L41==1,1,0)
# t$SchoolHoliday._L4a <- ifelse(t$SchoolHoliday._L42==1,1,0)
# t$SchoolHoliday._L4b <- ifelse(t$SchoolHoliday._L42==1,1,0)
# t$SchoolHoliday._L4c <- ifelse(t$SchoolHoliday._L42==1,1,0)
# 
# t$SchoolHoliday._L50 <- ifelse(t$SchoolHoliday._L51==1,1,0)
# t$SchoolHoliday._L5a <- ifelse(t$SchoolHoliday._L52==1,1,0)
# t$SchoolHoliday._L5b <- ifelse(t$SchoolHoliday._L52==1,1,0)
# t$SchoolHoliday._L5c <- ifelse(t$SchoolHoliday._L52==1,1,0)
# 
# t$Promo.2Valid_L10 <- ifelse(t$Promo.2Valid_L11==1,1,0)
# t$Promo.2Valid_L20 <- ifelse(t$Promo.2Valid_L21==1,1,0)

#save(t,file="Datasets/FinalTest.RData")

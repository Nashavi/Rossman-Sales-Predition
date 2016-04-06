
source("Modeling/Tree Modeling Setup.R")
load("Datasets/TreeFinalTestData.RData")
load("Modeling/GBM Models/GBMPreds.RData")

unique(finalPreds[finalPreds$Sales!=-1,"State"])

st <- "BEHBHHNISH"  # assign the state your are applying predictions for 
ntrees <- 4500
depth <- 5
shrink <- .1


d <- rbind(training,testing)
d <- rbind(training,eval)
#finalPreds <- TreeFinalTestData[,-c(5:84)]
#finalPreds$Sales <- -1
t <- TreeFinalTestData

rm(eval)
rm(testing)
rm(training)
rm(TreeFinalTestData)

d <- d[d$State==st & d$Open==1 & d$Sales!=0,]
d <- d[,-c(1:4)]
d$Sales <- log(d$Sales)

t$Open.0 <- ifelse(t$Open==0,1,0)
t$Open.1 <- ifelse(t$Open==1,1,0)
t <- t[,-c(1:4)]

t$Open._L11 <- ifelse(t$Open_L1.1==1,1,0)
t$Open._L12 <- ifelse(t$Open_L1.2==1,1,0)
t$Open._L13 <- ifelse(t$Open_L1.3==1,1,0)



gbm.mod <- gbm(Sales ~ .
               , data = d
               ,n.trees=ntrees
               ,interaction.depth =depth
               ,shrinkage=shrink
               ,distribution = "gaussian")

gbmPreds <- predict(gbm.mod
                    ,t
                    ,n.trees = ntrees
                    ,interaction.depth = depth
                    ,shrinkage = shink
                    ,type="response")


finalPreds[finalPreds$State==st,"Sales"] #check to see if there are sales values predicted already - should be 0's....


#conditionally update the sales figure 
#if the state matches, match the newsales figure, otherwise keep the value from the dataset. 
finalPreds$Sales <- ifelse(finalPreds$Open==1 & finalPreds$State==st,gbmPreds,finalPreds$Sales)

finalPreds$Sales <- ifelse(finalPreds$Open==0,0,finalPreds$Sales)

#now save the dataframe off...
save(finalPreds,file="Modeling/GBM Models/GBMPreds.RData")


write.csv(finalPreds,file="Prediction Files/April 6 Preds.csv")



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

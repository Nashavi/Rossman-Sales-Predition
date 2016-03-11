require(caret)

load("Datasets/GrandMasterData.RData")
load("Datasets/FinalTestData.RData")
#load("Datasets/Eval.RData")

d <- GrandMasterData
t <- FinalTestData
rm(GrandMasterData)
rm(FinalTestData)
d <- d[,-which(names(d) %in% c("dayofYear","weekday"))]

#t <- t[,-which(names(t) %in% c("Store","dayofYear","weekday","Date","StateCode","StateName"))]

#t <- t[order(t$Id),]
#t[t$Id==907,]

#t$State <- t$StateCode


# 
# states <- unique(d$State)
# states
# st <- "HE"
# d <- d[d$State==st,]
# # d<-d[,-1]
# # t <- t[t$State==st & t$Open==1,]
# # t <- t[,-1]

target="Sales"



set.seed(686)
inTraining <- createDataPartition(d$Sales, p = .6, list = FALSE)
training <- d[ inTraining,]
testing  <- d[-inTraining,]
inEval <- createDataPartition(testing$Sales,p=.5,list=FALSE)
eval <- testing[ inEval,]
testing <- testing[ -inEval,]

rm(inTraining)
rm(inEval)
rm(d)

Store <- training$Store
State <- training$State
Open <- training$Open
Sales <- training$Sales
training <- training[,-which(names(training) %in% c("Store","State","Open","Sales"))]
dmyCoding <- dummyVars(~.,data=training)
training <- data.frame(predict(dmyCoding,newdata = training))
training <- cbind(Store,training)
training <- cbind(State,training)
training <- cbind(Open,training)
training <- cbind(Sales,training)
save(training,file="Datasets/Training.RData",compress = TRUE)

Store <- testing$Store
State <- testing$State
Open <- testing$Open
Sales <- testing$Sales
testing <- testing[,-which(names(testing) %in% c("Store","State","Open","Sales"))]
testing <- data.frame(predict(dmyCoding,newdata = testing))
testing <- cbind(Store,testing)
testing <- cbind(State,testing)
testing <- cbind(Open,testing)
testing <- cbind(Sales,testing)
save(testing,file="Datasets/Testing.RData",compress = TRUE)

Store <- eval$Store
State <- eval$State
Open <- eval$Open
Sales <- eval$Sales
eval <- eval[,-which(names(eval) %in% c("Store","State","Open","Sales"))]
eval <- data.frame(predict(dmyCoding,newdata = eval))
eval <- cbind(Store,eval)
eval <- cbind(State,eval)
eval <- cbind(Open,eval)
eval <- cbind(Sales,eval)
save(eval,file="Datasets/Eval.RData",compress = TRUE)

rm(eval)
rm(training)
rm(testing)

Store <- t$Store
State <- t$State
Open <- t$Open
Id <- t$Id
t <- t[,-which(names(t) %in% c("Store","State","Open","Id"))]

# State Holiday == 0, Year = 2015
t <- t[,-which(names(t) %in% c("StateHoliday","year"))]
dmyCoding <- dummyVars(~.,data=t)
t <- data.frame(predict(dmyCoding,newdata = t))
t$StateHoliday.0 = 1
t$StateHoliday.1 = 0
t$year.2015 = 1
t$year.2014 = 0
t$year.2013 = 0



t <- cbind(Store,t)
t <- cbind(State,t)
t <- cbind(Open,t)
t <- cbind(Id,t)
FinalTestData <- t

save(FinalTestData,file="Datasets/FinalTestData.RData",compress = TRUE)


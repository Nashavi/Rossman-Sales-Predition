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
training <- training[,-which(names(training) %in% c("Store","State","Open"))]
dmyCoding <- dummyVars(Sales~.,data=training)
training <- data.frame(predict(dmyCoding,newdata = training))
training <- cbind(Store,training)
training <- cbind(State,training)
training <- cbind(Open,training)
save(training,file="Datasets/Training.RData",compress = TRUE)

Store <- testing$Store
State <- testing$State
Open <- testing$Open
testing <- testing[,-which(names(testing) %in% c("Store","State","Open"))]
testing <- data.frame(predict(dmyCoding,newdata = testing))
testing <- cbind(Store,testing)
testing <- cbind(State,testing)
testing <- cbind(Open,testing)
save(testing,file="Datasets/Testing.RData",compress = TRUE)

Store <- eval$Store
State <- eval$State
Open <- eval$Open
eval <- eval[,-which(names(eval) %in% c("Store","State","Open"))]
eval <- data.frame(predict(dmyCoding,newdata = eval))
eval <- cbind(Store,eval)
eval <- cbind(State,eval)
eval <- cbind(Open,eval)
save(eval,file="Datasets/Eval.RData",compress = TRUE)


eval <- data.frame(predict(dmy,newdata = eval))

testing <- cbind(testing,Store)
testing <- cbind(testing,State)

save(testing,file="Datasets/Testing.RData",compress = TRUE)

rm(testing)

FinalTestData <- t

save(FinalTestData,file="Datasets/FinalTestDummified.RData",compress = TRUE)



training <- cbind(training,state)
training$state




testing <- data.frame(predict(dmy,newdata = testing))
eval <- data.frame(predict(dmy,newdata = eval))


save(training,file="Datasets/Training.RData",compress = TRUE)
save(testing,file="Datasets/Testing.RData",compress=TRUE)
save(eval,file="Datasets/Eval.RData",compress = TRUE)
rm(training)
rm(testing)
rm(eval)

load("Datasets/Eval.RData")
head(eval$year.2013)


load("Modeling/ST_gbmModel.RData")

test.preds <- predict(gbmFit,newdata = t)

t$StateHoliday.0 <- 1
t$StateHoliday.1 <- 0
t$year.2013 <- 0
t$year.2014 <- 0
t$year.2015 <- 1


t <- t[,-which(names(t) %in% c("StateHoliday","year"))]

dmy <- dummyVars("~.",data=eval)
t <- data.frame(predict(dmy,newdata = t))








rm(dmy)
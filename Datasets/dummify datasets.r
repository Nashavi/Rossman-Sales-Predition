require(caret)

load("Datasets/TrainData.RData")
#load("Datasets/GrandMasterData.RData")
load("Datasets/FinalTestData.RData")
#load("Datasets/Eval.RData")

d <- TrainData
rm(TrainData)
#d <- GrandMasterData
t <- FinalTestData
#rm(GrandMasterData)
rm(FinalTestData)
#d <- d[,-which(names(d) %in% c("dayofYear","weekday"))]

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

d <- d[,-3]
d <- d[d$quarter %in% c(2,3),]
d<- d[,-which(names(d) %in% c("Promo2"))]

levels(t$Open_L1) <- levels(d$Open_L1)
levels(t$promovalid_L1) <- levels(d$promovalid_L1)
levels(t$promovalid_L2) <- levels(d$promovalid_L2)
levels(t$StateHoliday_L1) <- levels(d$StateHoliday_L1)
levels(t$StateHoliday_L2) <- levels(d$StateHoliday_L2)
levels(t$StateHoliday_L3) <- levels(d$StateHoliday_L3)
levels(t$StateHoliday_L4) <- levels(d$StateHoliday_L4)
levels(t$StateHoliday_L5) <- levels(d$StateHoliday_L5)
levels(t$SchoolHoliday_L1) <- levels(d$SchoolHoliday_L1)
levels(t$SchoolHoliday_L2) <- levels(d$SchoolHoliday_L2)
levels(t$SchoolHoliday_L3) <- levels(d$SchoolHoliday_L3)
levels(t$SchoolHoliday_L4) <- levels(d$SchoolHoliday_L4)
levels(t$SchoolHoliday_L5) <- levels(d$SchoolHoliday_L5)
levels(t$Promo2Valid_L1) <- levels(d$promovalid_L1)
levels(t$promovalid_L2) <- levels(d$promovalid_L2)




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

X <- training$X
Store <- training$Store
State <- training$State
Open <- training$Open
Sales <- training$Sales
training <- training[,-which(names(training) %in% c("X","Store","State","Open","Sales"))]
dmyCoding <- dummyVars(~.,data=training)
training <- data.frame(predict(dmyCoding,newdata = training))
training <- cbind(Store,training)
training <- cbind(State,training)
training <- cbind(Open,training)
training <- cbind(Sales,training)
training <- cbind(X,training)
save(training,file="Datasets/Training.RData",compress = TRUE)

X <- testing$X
Store <- testing$Store
State <- testing$State
Open <- testing$Open
Sales <- testing$Sales
testing <- testing[,-which(names(testing) %in% c("X","Store","State","Open","Sales"))]
testing <- data.frame(predict(dmyCoding,newdata = testing))
testing <- cbind(Store,testing)
testing <- cbind(State,testing)
testing <- cbind(Open,testing)
testing <- cbind(Sales,testing)
testing <- cbind(X,testing)
save(testing,file="Datasets/Testing.RData",compress = TRUE)

X <- eval$X
Store <- eval$Store
State <- eval$State
Open <- eval$Open
Sales <- eval$Sales
eval <- eval[,-which(names(eval) %in% c("X","Store","State","Open","Sales"))]
eval <- data.frame(predict(dmyCoding,newdata = eval))
eval <- cbind(Store,eval)
eval <- cbind(State,eval)
eval <- cbind(Open,eval)
eval <- cbind(Sales,eval)
eval <- cbind(X,eval)
save(eval,file="Datasets/Eval.RData",compress = TRUE)


X <- t$Id
Store <- t$Store
State <- t$State
Open <- t$Open

t <- t[,-which(names(t) %in% c("Id","Store","State","Open"))]
t <- data.frame(predict(dmyCoding,newdata = t))
t <- cbind(Store,t)
t <- cbind(State,t)
t <- cbind(Open,t)
t <- cbind(Sales,t)
t <- cbind(X,t)
save(t,file="Datasets/FinalTest.RData",compress = TRUE)








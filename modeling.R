d <- read.csv("Datasets/GrandMasterData.csv",stringsAsFactors = TRUE,na.strings = c("","NA"))

sqrt(var(d$Sales))

d$Store <- as.factor(d$Store)
d$Date <- as.Date(d$Date)
d$Open <- as.factor(d$Open)
d$Promo <- as.factor(d$Promo)
d$StateHoliday <- as.factor(ifelse(d$StateHoliday=="0","0","1"))
d$SchoolHoliday <- as.factor(ifelse(d$SchoolHoliday=="0","0","1"))
d$dayofYear <- as.factor(d$dayofYear)
d$dayofWeek <- as.factor(d$dayofWeek)
d$dayofMonth <- as.factor(d$dayofMonth)
d$weekNum <- as.factor(d$weekNum)
d$month <- as.factor(d$month)
d$quarter <- as.factor(d$quarter)
d$year <- as.factor(d$year)
d$CompetitionOpenSinceMonth <- as.factor(d$CompetitionOpenSinceMonth)
d$CompetitionOpenSinceYear <- as.factor(d$CompetitionOpenSinceYear)
d$Promo2 <- as.factor(d$Promo2)
d$WeeksSinceCompOpened <- as.factor(d$WeeksSinceCompOpened)
d$Promo2Valid <- as.factor(d$Promo2Valid)
d$promovalid <- as.factor(d$promovalid)
d$Open_L1 <- as.factor(d$Open_L1)
d$promovalid_L1 <- as.factor(d$promovalid_L1)
d$promovalid_L2 <- as.factor(d$promovalid_L2)
d$Promo2Valid_L1 <- as.factor(d$Promo2Valid_L1)
d$Promo2Valid_L2 <- as.factor(d$Promo2Valid_L2)

states <- unique(d$State)
s <- sample(states,1,replace=FALSE)
stores <- unique(d[d$State==s,"Store"])

st <- sample(stores,5,replace = FALSE)

t<-d[d$State==s & d$Store %in% st & d$Open==1,]

t<- t[,-which(names(t) %in% c("X","Open","CompetitionOpenSinceMonth","CompetitionOpenSinceYear"))]
t<- t[,-which(names(t) %in% c("Promo2","Promo2SinceWeek","Promo2SinceYear","PromoInterval"))]
t<- t[,-which(names(t) %in% c("CompetitionOpenDate","Date","Promo2StartDate","SalesMonthInterval"))]
t<- t[,-which(names(t) %in% c("Name","Customers","State","AirportCode","Store"))]

require(randomForest)
require(caret)
require(ggplot2)

set.seed(686)
inTraining <- createDataPartition(t$Sales, p = .6, list = FALSE)
training <- t[ inTraining,]
testing  <- t[-inTraining,]
inEval <- createDataPartition(testing$Sales,p=.5,list=FALSE)
eval <- testing[ inEval,]
testing <- testing[ -inEval,]

# m = matrix(data=NA,nrow=length(names(training)),ncol=2)
# dimnames(m) = list(names(training),c("Total Missing","Percent Missing"))        
# for(i in 1:nrow(m)){
#   l = length(training[,i])
#   u = length(which(is.na(training[,i])==TRUE))
#   m[i,"Total Missing"] = u
#   m[i,"Percent Missing"] = round(u/l,digits=4)
# }
# m[m[,"Percent Missing"]!=0,]

dmy <- dummyVars("~ .",data=training)
training <- data.frame(predict(dmy,newdata = training))
rm(dmy)

training$Sales <- log(training$Sales)

fitControl = trainControl(method = 'cv'
                          , number=5
                          ,summaryFunction=defaultSummary) 
gbmGrid =  expand.grid(interaction.depth= seq(1,3,by=1), #which tree depth values to try
                       n.trees = seq(100,1000,by=100), #how many values of n.trees to try
                       shrinkage = 0.1,
                       n.minobsinnode=10)
gbmFit = train(Sales ~ .
               ,data = training
               ,method='gbm'
               ,trControl=fitControl
               ,metric="RMSE"
               ,tuneGrid=gbmGrid
               ,verbose=FALSE) 

summary(gbmFit)[1:10,]
gbmFit$bestTune #what was the best tune?
plot(gbmFit)
r = which.min(gbmFit$results[,"RMSE"])#which combo had the best RMSE?
ntrees = gbmFit$results[r,"n.trees"] #what was the # of trees?
depth = gbmFit$results[r,"interaction.depth"] #how deep were the trees?
shrink = gbmFit$results[r,"shrinkage"] #what was the shrinkage?

dmy <- dummyVars("~ .",data=testing)
testing <- data.frame(predict(dmy,newdata = testing))
rm(dmy)

testing$Sales <- log(testing$Sales)

gmb.model = gbm(Sales~., data=testing,n.trees=ntrees,interaction.depth=depth,shrinkage=shrink
                ,distribution = "gaussian")

dmy <- dummyVars("~ .",data=eval)
eval <- data.frame(predict(dmy,newdata = eval))
rm(dmy)

eval$Sales <- log(eval$Sales)

gbm.preds <- predict(gmb.model, eval, n.trees=ntrees,interaction.depth=depth,shrinkage=shrink)

mean(sqrt((gbm.preds-eval$Sales)^2))

# full.model = lm(Sales~.
#                 ,data=train)
# null.model = lm(Sales~1,data=train)
# step.model = step(null.model,scope=list(upper=full.model),data=train,direction="both")
# 
# step.aic = as.data.frame(step.model$anova,row.names = c(rep(1:nrow(step.model$anova)))) #dump results to a data frame
# step.aic
# step.aic$variables_added = rep(1:nrow(step.model$anova)) #add an column for each variable added
# ggplot(step.aic,aes(variables_added,AIC))+geom_point()+geom_line(color="red")+ggtitle("Stepwise AIC by Total Variables Added")+xlim(0,13)
# 
# 

# require(rpart)
# 
# tree.mod = rpart(Sales~.-Sales, data = training)
# names(tree.mod)
# tree.mod$variable.importance
# plot(tree.mod$variable.importance,xlab = names(train))
# tmv <- as.data.frame(tree.mod$variable.importance)
# 
# printcp(tree.mod)
# plotcp(tree.mod)
# 
# min.cp = tree.mod$cptable[which.min(tree.mod$cptable[,"xerror"]),"CP"] #which tree has the min cp?
# p.tree.mod = prune(tree.mod,cp=min.cp) 
# 
# plot(p.tree.mod, compress = TRUE)
# text(p.tree.mod, use.n = TRUE)
# 
# p.tree.mod$variable.importance
# summary(p.tree.mod)
# 
# 
# pr.tree <- predict(p.tree.mod,testing)
# 
# require(cvTools)
# 
# rmspe(testing$Sales, predict(p.tree.mod))
# sqrt(mean((pr.tree - testing$Sales)^2))
# sqrt(var(testing$Sales))
# 
# td <- read.csv("Datasets/FinalTest.csv",stringsAsFactors = TRUE,na.strings = c("","NA"))
# td <- td[,-1]
# td$Date <- as.Date(td$Date,'%m/%d/%Y')
# td$Store <- as.factor(td$Store)
# td$Open <- as.factor(td$Open)
# td$Promo <- as.factor(td$Promo)
# td$StateHoliday <- as.factor(td$StateHoliday)
# td$SchoolHoliday <- as.factor(td$SchoolHoliday)
# td$StoreType <- as.factor(td$StoreType)
# td$Assortment <- as.factor(td$Assortment)
# td$CompetitionOpenSinceMonth <- as.factor(td$CompetitionOpenSinceMonth)
# td$CompetitionOpenSinceYear <- as.factor(td$CompetitionOpenSinceYear)
# td$Promo2 <- as.factor(td$Promo2)
# td$Promo2SinceWeek <- as.factor(td$Promo2SinceWeek)
# td$Promo2SinceYear <- as.factor(td$Promo2SinceYear)
# td$CompetitionOpenDate <- as.Date(td$CompetitionOpenDate,'%m/%d/%Y')
# td$Promo2StartDate <- as.Date(td$Promo2StartDate,'%m/%d/%Y')
# td$dayofYear <- as.factor(td$dayofYear)
# td$dayofWeek <- as.factor(td$dayofWeek)
# td$weekday <- as.factor(td$weekday)
# td$dayofMonth <- as.factor(td$dayofMonth)
# td$weekNum <- as.factor(td$weekNum)
# td$month <- as.factor(td$month)
# td$quarter <- as.factor(td$quarter)
# td$year <- as.factor(td$year)
# td$Promo2Valid <- as.factor(td$Promo2Valid)
# td$promovalid <- as.factor(td$promovalid)
# td$Open_L1 <- as.factor(td$Open_L1)
# td$promovalid_L1 <- as.factor(td$promovalid_L1)
# td$promovalid_L2 <- as.factor(td$promovalid_L2)
# td$StateHoliday_L1 <- as.factor(td$StateHoliday_L1)
# td$StateHoliday_L2 <- as.factor(td$StateHoliday_L2)
# td$StateHoliday_L3 <- as.factor(td$StateHoliday_L3)
# td$StateHoliday_L4 <- as.factor(td$StateHoliday_L4)
# td$StateHoliday_L5 <- as.factor(td$StateHoliday_L5)
# td$SchoolHoliday_L1 <- as.factor(td$SchoolHoliday_L1)
# td$SchoolHoliday_L2 <- as.factor(td$SchoolHoliday_L2)
# td$SchoolHoliday_L3 <- as.factor(td$SchoolHoliday_L3)
# td$SchoolHoliday_L4 <- as.factor(td$SchoolHoliday_L4)
# td$SchoolHoliday_L5 <- as.factor(td$SchoolHoliday_L5)
# td$promovalid_L1 <- as.factor(td$promovalid_L1)
# td$promovalid_L2 <- as.factor(td$promovalid_L2)
# td$StateHoliday <- as.factor(td$StateHoliday)
# 
# td<-
# 
# td$yhat <- ifelse(td$Open=="0",0, predict(p.tree.mod,td))
# 
# levels(train$SchoolHoliday)
# levels(td$SchoolHoliday)
# 
# test.data$WeeksSinceCompOpened <- as.integer(difftime(test.data$Date,test.data$CompetitionOpenDate,units="weeks"))
# #if the comp wasn't open yet, set the value to 0 
# test.data$WeeksSinceCompOpened <- ifelse(test.data$WeeksSinceCompOpened < 0, 0,test.data$WeeksSinceCompOpened)
# 
# write.csv(td,"Datasets/FinalTest.csv")
# 
# rm(min.cp)
# rm(p.tree.mod)
# rm(tree.mod)
# 
# attach(train)
# train <- train[is.na(train$Sales)==FALSE,]
# 
# p = dim(t)[2]-1
# p = round(sqrt(11),0)
# rf.mod = randomForest(Sales~ Promo + StateHoliday + dayofWeek + weekday + dayofMonth
#                       + weekNum + month + quarter + StoreType + Assortment + State
#                       ,data=train,mtry=p,ntrees=1000
#                       )
# #print(rf.mod)
# train$Sales
# 
# 
# training <- training[,-1]


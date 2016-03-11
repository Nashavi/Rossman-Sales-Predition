d <- read.csv("Datasets/GrandMasterData.csv")
str(d)
d[sapply(d, is.character)] <- lapply(d[sapply(d, is.character)],as.factor)
d<- d[,-1]
d$Store <- as.factor(d$Store)
d$Date <- as.Date(d$Date)
d$Open <- as.factor(d$Open)
d$Promo <- as.factor(d$Promo)
d$StateHoliday <- as.character(d$StateHoliday)
d$StateHoliday <- as.factor(ifelse(d$StateHoliday=="0","0","1"))
d$SchoolHoliday <- as.character(d$SchoolHoliday)
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
d$Promo2Valid <- as.factor(d$Promo2Valid)
d$promovalid <- as.factor(d$promovalid)
d$Open_L1 <- as.factor(d$Open_L1)
d$promovalid_L1 <- as.factor(d$promovalid_L1)
d$promovalid_L2 <- as.factor(d$promovalid_L2)
d$Promo2Valid_L1 <- as.factor(d$Promo2Valid_L1)
d$Promo2Valid_L2 <- as.factor(d$Promo2Valid_L2)

states <- unique(d$State)
states
#s <- sample(states,1,replace=FALSE)
s <- "HE"
stores <- unique(d[d$State==s,"Store"])

st <- stores  #sample(stores,10,replace = FALSE)

st

### hmmmmm well there might still be $0 sales in this dataframe....that would f up everything!
t <-d[d$State==s & d$Store %in% st & d$Open==1 &d$Sales!=0,]

t<- t[,-which(names(t) %in% c("X","Open","CompetitionOpenSinceMonth","CompetitionOpenSinceYear"))]
t<- t[,-which(names(t) %in% c("Promo2","Promo2SinceWeek","Promo2SinceYear","PromoInterval"))]
t<- t[,-which(names(t) %in% c("CompetitionOpenDate","Date","Promo2StartDate","SalesMonthInterval"))]
t<- t[,-which(names(t) %in% c("Name","Customers","State","AirportCode","Store"))]
#t<- t[,-which(names(t) %in% c("dayofYear","weekNum","events"))]

m = matrix(data=NA,nrow=length(names(t)),ncol=2)
dimnames(m) = list(names(t),c("Total Missing","Percent Missing"))        
for(i in 1:nrow(m)){
  l = length(t[,i])
  u = length(which(is.na(t[,i])))
  m[i,"Total Missing"] = u
  m[i,"Percent Missing"] = round(u/l,digits=4)
}
m <- data.frame(m[m[,"Percent Missing"]!=0,])

random.imp <- function(a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs,n.missing,replace=TRUE)
  return (imputed)
}

t$CompetitionDistance <- ifelse(is.na(t$CompetitionDistance)==TRUE,
                                random.imp(t$CompetitionDistance),
                                t$CompetitionDistance)
t$WeeksSinceCompOpened <- ifelse(is.na(t$WeeksSinceCompOpened)==TRUE,
                                random.imp(t$WeeksSinceCompOpened),
                                t$WeeksSinceCompOpened)

t$Promo2Valid <- as.character(t$Promo2Valid)
t$Promo2Valid <- as.factor(ifelse(is.na(t$Promo2Valid)==TRUE,"2",t$Promo2Valid))

t$cloudcover <- as.factor(ifelse(is.na(t$cloudcover)==TRUE,
                                 random.imp(t$cloudcover),
                                 t$cloudcover))
t$events <- as.factor(ifelse(is.na(t$events)==TRUE,
                       random.imp(t$events),
                       t$cloudcover))
t$Open_L1 <- as.character(t$Open_L1)
t$Open_L1 <- as.factor(ifelse(is.na(t$Open_L1)==TRUE,"2",t$Open_L1))
t$promovalid_L1 <- as.character(t$promovalid_L1)
t$promovalid_L1 <- as.factor(ifelse(is.na(t$promovalid_L1)==TRUE,"2",t$promovalid_L1))
t$promovalid_L2 <- as.character(t$promovalid_L2)
t$promovalid_L2 <- as.factor(ifelse(is.na(t$promovalid_L2)==TRUE,"2",t$promovalid_L2))
t$StateHoliday_L1 <- as.character(t$StateHoliday_L1)
t$StateHoliday_L1 <- as.factor(ifelse(is.na(t$StateHoliday_L1)==TRUE,"2",t$StateHoliday_L1))
t$StateHoliday_L2 <- as.character(t$StateHoliday_L2)
t$StateHoliday_L2 <- as.factor(ifelse(is.na(t$StateHoliday_L2)==TRUE,"2",t$StateHoliday_L2))
t$StateHoliday_L3 <- as.character(t$StateHoliday_L3)
t$StateHoliday_L3 <- as.factor(ifelse(is.na(t$StateHoliday_L3)==TRUE,"2",t$StateHoliday_L3))
t$StateHoliday_L4 <- as.character(t$StateHoliday_L4)
t$StateHoliday_L4 <- as.factor(ifelse(is.na(t$StateHoliday_L4)==TRUE,"2",t$StateHoliday_L4))
t$StateHoliday_L5 <- as.character(t$StateHoliday_L5)
t$StateHoliday_L5 <- as.factor(ifelse(is.na(t$StateHoliday_L5)==TRUE,"2",t$StateHoliday_L5))
t$SchoolHoliday_L1 <- as.character(t$SchoolHoliday_L1)
t$SchoolHoliday_L1 <- as.factor(ifelse(is.na(t$SchoolHoliday_L1)==TRUE,"2",t$SchoolHoliday_L1))
t$SchoolHoliday_L2 <- as.character(t$SchoolHoliday_L2)
t$SchoolHoliday_L2 <- as.factor(ifelse(is.na(t$SchoolHoliday_L2)==TRUE,"2",t$SchoolHoliday_L2))
t$SchoolHoliday_L3 <- as.character(t$SchoolHoliday_L3)
t$SchoolHoliday_L3 <- as.factor(ifelse(is.na(t$SchoolHoliday_L3)==TRUE,"2",t$SchoolHoliday_L3))
t$SchoolHoliday_L4 <- as.character(t$SchoolHoliday_L4)
t$SchoolHoliday_L4 <- as.factor(ifelse(is.na(t$SchoolHoliday_L4)==TRUE,"2",t$SchoolHoliday_L4))
t$SchoolHoliday_L5 <- as.character(t$SchoolHoliday_L5)
t$SchoolHoliday_L5 <- as.factor(ifelse(is.na(t$SchoolHoliday_L5)==TRUE,"2",t$SchoolHoliday_L5))
t$Promo2Valid_L1 <- as.character(t$Promo2Valid_L1)
t$Promo2Valid_L1 <- as.factor(ifelse(is.na(t$Promo2Valid_L1)==TRUE,"2",t$Promo2Valid_L1))
t$Promo2Valid_L2 <- as.character(t$Promo2Valid_L2)
t$Promo2Valid_L2 <- as.factor(ifelse(is.na(t$Promo2Valid_L2)==TRUE,"2",t$Promo2Valid_L2))

m = matrix(data=NA,nrow=length(names(t)),ncol=2)
dimnames(m) = list(names(t),c("Total Missing","Percent Missing"))        
for(i in 1:nrow(m)){
  l = length(t[,i])
  u = length(which(is.na(t[,i])))
  m[i,"Total Missing"] = u
  m[i,"Percent Missing"] = round(u/l,digits=4)
}
m <- data.frame(m[m[,"Percent Missing"]!=0,])
rm(m)
require(randomForest)
require(caret)
require(ggplot2)
require(cvTools)

set.seed(686)
inTraining <- createDataPartition(t$Sales, p = .6, list = FALSE)
training <- t[ inTraining,]
testing  <- t[-inTraining,]
inEval <- createDataPartition(testing$Sales,p=.5,list=FALSE)
eval <- testing[ inEval,]
testing <- testing[ -inEval,]

dmy <- dummyVars("~ .",data=training)
training <- data.frame(predict(dmy,newdata = training))
rm(dmy)

training$Sales <- log(training$Sales)

#nsv<-data.frame(nearZeroVar(training, saveMetrics=TRUE))
#nsv[nsv$zeroVar==TRUE,]
#nsv[nsv$nzv==TRUE,]

#training <- training[,-which(names(training) %in% c("dayofWeek.0","weekday.Sun","StoreType.b","Assortment.b"))]



# fitControl = trainControl(method = 'cv'
#                           , number=3
#                           ,summaryFunction=oldSumm)
# gbmGrid =  expand.grid(interaction.depth= seq(2,3,by=1), #which tree depth values to try
#                        n.trees = seq(4000,5000,by=200), #how many values of n.trees to try
#                        shrinkage = c(0.1),
#                        n.minobsinnode=20)
# gbmFit = train(Sales ~ .
#                ,data = training
#                ,method='gbm'
#                ,trControl=fitControl
#                ,metric="RMSE"
#               
#                ,tuneGrid=gbmGrid
#                ,verbose=TRUE) 

summary(gbmFit)[1:10,]
gbmFit$bestTune #what was the best tune?
plot(gbmFit)
gbmFit$results
r = which.min(gbmFit$results[,"RMSE"])#which combo had the best RMSE?
ntrees = gbmFit$results[r,"n.trees"] #what was the # of trees?
depth = gbmFit$results[r,"interaction.depth"] #how deep were the trees?
shrink = gbmFit$results[r,"shrinkage"] #what was the shrinkage?

dmy <- dummyVars("~ .",data=testing)
testing <- data.frame(predict(dmy,newdata = testing))
rm(dmy)
dmy <- dummyVars("~ .",data=eval)
eval <- data.frame(predict(dmy,newdata = eval))
rm(dmy)

testing$Sales <- log(testing$Sales)

test.preds <- predict(gbmFit,newdata = testing)
  #predict(testing$Sales, gbmFit, n.trees=ntrees,interaction.depth=depth,shrinkage=shrink)
eval.preds <- predict(gbmFit,newdata = eval)
  #predict(gmb.model, eval, n.trees=ntrees,interaction.depth=depth,shrinkage=shrink)
eval$Sales <- log(eval$Sales)

sqrt(mean((testing$Sales-test.preds)^2))
sqrt(mean((eval$Sales-eval.preds)^2))

save(gbmFit,file="ST_gbmModel.RData",compress = TRUE)



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


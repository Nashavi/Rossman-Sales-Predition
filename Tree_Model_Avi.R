rm(list=ls())
require(rpart)
require(caret)
d<-read.csv("Datasets/GrandMasterData.csv")

d$Store<-as.factor(d$Store)
d$Open<-as.factor(d$Open)
d$Promo<-as.factor(d$Promo)
d$dayofYear<-as.factor(d$dayofYear)
d$dayofWeek<-as.factor(d$dayofWeek)
d$dayofMonth<-as.factor(d$dayofMonth)
d$weekNum<-as.factor(d$weekNum)
d$month<-as.factor(d$month)
d$quarter<-as.factor(d$quarter)
d$year<-as.factor(d$year)
d$CompetitionOpenSinceMonth<-as.factor(d$CompetitionOpenSinceMonth)
d$CompetitionOpenSinceYear<-as.factor(d$CompetitionOpenSinceYear)
d$Promo2<-as.factor(d$Promo2)
d$Promo2SinceWeek<-as.factor(d$Promo2SinceWeek)
d$Promo2SinceYear<-as.factor(d$Promo2SinceYear)
d$WeeksSinceCompOpened<-as.factor(d$WeeksSinceCompOpened)
d$Promo2Valid<-as.factor(d$Promo2Valid)
d$promovalid<-as.factor(d$promovalid)
d$Open_L1<-as.factor(d$Open_L1)
d$promovalid_L1<-as.factor(d$promovalid_L1)
d$promovalid_L2<-as.factor(d$promovalid_L2)
d$Promo2Valid_L1<-as.factor(d$Promo2Valid_L1)
d$Promo2Valid_L2<-as.factor(d$Promo2Valid_L2)


trainindex<-createDataPartition(d$Date, p = .8,list = FALSE,times = 1)

dtrain<-d[trainindex,]
dtest<-d[-trainindex,]



colSums(is.na(dtrain))
dtrain$Date<-NULL #Date was not giving a right NA error while growing the tree. SO had to NULL the date column in the training set
dtrain$Open<-NULL #Open was not giving a right NA error while growing the tree. SO had to NULL the date column in the training set

dtrain$Customers<-NULL #Removing features not in test set
dtrain$WeeksSinceCompOpened<-NULL #Removing features not in test set
dtrain$SalesMonthInterval<-NULL #Removing features not in test set
dtrain$Name<-NULL #Removing features not in test set

fit<-rpart(Sales ~.,data = dtrain, method = "anova", control=rpart.control(minsplit= 30,cp = 0.001),na.action=na.pass)

printcp(fit)
plotcp(fit)
summary(fit)$variableimportance

plot(fit, uniform=TRUE,main="Regression Tree for Sales")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) 
dev.off()

min.cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

p.tree.mod = prune(fit,cp=min.cp)
printcp(p.tree.mod)

dtest$yvalues<-0
dtest$yvalues<-ifelse(dtest$Open!=0,predict(p.tree.mod,dtest),0)
head(dtest$yvalues)
mean((dtest$yvalues-dtest$Sales)^2)
sqrt(mean((dtest$yvalues-dtest$Sales)^2))

par(mfrow=c(1,2));hist(dtest$yvalues);hist(dtest$Sales)
dev.off()
boxplot(dtest$Sales,dtest$yvalues,names=c("Sales","Yvalues"))

mean(dtest$Sales)
mean(dtest$yvalues)
sqrt(var(dtest$yvalues))
sqrt(var(dtest$Sales))


dtest$ydiff<-dtest$yvalues-dtest$Sales
ggplot(dtest,aes(Date,ydiff))+geom_point()

sum(dtest$yvalues[dtest$Open==0])#check if closed have 0 sales

test<-read.csv("Datasets/FinalTest.csv")

test$Store<-as.factor(test$Store)
test$Open<-as.factor(test$Open)
test$Promo<-as.factor(test$Promo)
test$dayofYear<-as.factor(test$dayofYear)
test$dayofWeek<-as.factor(test$dayofWeek)
test$dayofMonth<-as.factor(test$dayofMonth)
test$weekNum<-as.factor(test$weekNum)
test$month<-as.factor(test$month)
test$quarter<-as.factor(test$quarter)
test$year<-as.factor(test$year)
test$CompetitionOpenSinceMonth<-as.factor(test$CompetitionOpenSinceMonth)
test$CompetitionOpenSinceYear<-as.factor(test$CompetitionOpenSinceYear)
test$Promo2<-as.factor(test$Promo2)
test$Promo2SinceWeek<-as.factor(test$Promo2SinceWeek)
test$Promo2SinceYear<-as.factor(test$Promo2SinceYear)
#test$WeeksSinceCompOpened<-as.factor(test$WeeksSinceCompOpened)
test$Promo2Valid<-as.factor(test$Promo2Valid)
test$promovalid<-as.factor(test$promovalid)
test$Open_L1<-as.factor(test$Open_L1)
test$promovalid_L1<-as.factor(test$promovalid_L1)
test$promovalid_L2<-as.factor(test$promovalid_L2)
test$Promo2Valid_L1<-as.factor(test$Promo2Valid_L1)
test$Promo2Valid_L2<-as.factor(test$Promo2Valid_L2)

test$StateHoliday<-as.factor(test$StateHoliday)
levels(test$StateHoliday)<-c("0","a","b","c")
test$SchoolHoliday<-as.factor(test$SchoolHoliday)
levels(test$SchoolHoliday)<-c("0","a","b","c")

test$State<-test$StateCode #Matching featurename to train set

Preds<-ifelse(test$Open!=0,predict(p.tree.mod,test),0)
write.csv(Preds,file="BaseLinePredictions.csv")

str(dtrain$StateHoliday)
str(test$StateHoliday)

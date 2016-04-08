
#load these datasets, lag the sales by 7,8,9,10 weeks. 
#save as new features
# save the datasets back down 

load("Datasets/TreeTraining.RData")
load("Datasets/TreeTesting.RData")
load("Datasets/TreeEval.RData")
require(DataCombine)

load("Datasets/TrainData.RData")

d <- training

#update this for a given lag
weeks <- 6 
lag <- weeks * 7 
#chang new variable name

dlagged <- slide(d, Var = "Sales", GroupVar = "Store", NewVar= "SixWeekSalesLag",slideBy = -lag)


lag.sales <- TrainData[,c("Store","Date","Sales")]
rm(TrainData)
lag.sales$sixWeeks <- lag.sales$Date + lag
lag<-lag+7
lag.sales$sevenWeeks <- lag.sales$Date + lag
lag<-lag+7
lag.sales$eightWeeks <- lag.sales$Date + lag
lag<-lag+7
lag.sales$nineWeeks <- lag.sales$Date + lag
lag<-lag+7
lag.sales$tenWeeks <- lag.sales$Date + lag

save(lag.sales,file="Datasets/LaggedTrainSales.RData",compress = TRUE)

testing$Date <- as.Date(paste(testing$year,testing$month,testing$dayofMonth,sep = "-"))

testing$SevenWeeksSalesLag <- -1
testing$EightWeeksSalesLag <- -1
testing$NineWeeksSalesLag <- -1
testing$TenWeeksSalesLag <- -1

testing$val <- testing$SixWeekSalesLag

for(i in 1:(length(testing$X))){
  st <- testing$Store[i]
  dt <- testing$Date[i]
  six <- lag.sales[lag.sales$Store==st & lag.sales$sixWeeks==dt,"Sales"]
  seven <- lag.sales[lag.sales$Store==st & lag.sales$sevenWeeks==dt,"Sales"]
  eight <- lag.sales[lag.sales$Store==st & lag.sales$eightWeeks==dt,"Sales"]
  nine <- lag.sales[lag.sales$Store==st & lag.sales$nineWeeks==dt,"Sales"]
  ten <- lag.sales[lag.sales$Store==st & lag.sales$tenWeeks==dt,"Sales"]
  print(paste(i,st,dt,six,seven,eight,nine,ten))
  training$SixWeekSalesLag[i] <- six
  training$SevenWeekSalesLag[i] <- seven
  training$EightWeekSalesLag[i] <- eight
  training$NineWeekSalesLag[i] <- nine
  training$TenWeekSalesLag[i] <- ten
}




lag<-lag+7
dlagged <- slide(dlagged, Var = "Sales", GroupVar = "Store", NewVar= "SevenWeekSalesLag",slideBy = -lag)

lag<-lag+7
dlagged <- slide(dlagged, Var = "Sales", GroupVar = "Store", NewVar= "EightWeekSalesLag",slideBy = -lag)

lag<-lag+7
dlagged <- slide(dlagged, Var = "Sales", GroupVar = "Store", NewVar= "NineWeekSalesLag",slideBy = -lag)

lag<-lag+7
dlagged <- slide(dlagged, Var = "Sales", GroupVar = "Store", NewVar= "TenWeekSalesLag",slideBy = -lag)

training <- dlagged

save(training,file="Datasets/TreeTraining.RData",compress = TRUE)


##### For TEst Dataset

# lag.stores <- unique(d[is.na(d$SixWeekSalesLag),"Store"])
# 
# for(i in 1:length(lag.stores)){
#   avg.sales <- mean(d[d$Store==i & d$year==2013 & d$month==1,"Sales"])
#   d[d$Store==i & is.na(d$SixWeekSalesLag)==TRUE,"SixWeekSalesLag"] <- avg.sales
#   print(paste(i,avg.sales))
# }
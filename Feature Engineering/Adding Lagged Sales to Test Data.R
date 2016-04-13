
require(DataCombine)

load("Datasets/TreeFinalTestData.RData")

load("Datasets/TreeTraining.RData")
load("Datasets/TreeTesting.RData")
load("Datasets/TreeEval.RData")

d <- rbind(eval,testing)
d <- rbind(d,training)

rm(training)
rm(testing)
rm(eval)

load("Datasets/Test.RData")
TreeFinalTestData$year <- 2015
for(i in (1:length(TreeFinalTestData$X))){
  m <- as.numeric(as.character(TestData[TestData$Id==i,"month"]))
  #print(paste(i,m))
  #print(TreeFinalTestData[TreeFinalTestData$X==i,"month"])
  TreeFinalTestData[TreeFinalTestData$X==i,"month"] <- m
}



TreeFinalTestData$Date <- as.Date(
  paste(TreeFinalTestData$year,TreeFinalTestData$month
        ,TreeFinalTestData$dayofMonth,sep = "-"),'%Y-%m-%d')

tmin <- which.min(TreeFinalTestData$Date)
TreeFinalTestData$Date[tmin]

tmax <- which.max(d$Date)
d$Date[tmax]
###### boundry is at 2015-06-19 to 2015-06-20

d <- d[,-which(names(d) %in% c("SixWeekSalesLag","SevenWeekSalesLag","EightWeekSalesLag","NineWeekSalesLag","TenWeekSalesLag"))]

d <- d[,c(1:7)]

t <- TreeFinalTestData 
rm(TreeFinalTestData)
rm(TestData)
Date <- t$Date
t <- t[,c(1:4)]
t <- cbind(Date,t)
t$Sales <- 0
t$Group <- 0
data <- rbind(d,t)

data$TestFlag <- ifelse(data$Date>"2015-06-19",1,0)
unique(data[data$Date<"2015-06-20","TestFlag"])

data<-data[order(data$Store,data$Date),] #order by store and then dtate

lag = 6*7
dlagged <- slide(data, Var = "Sales", GroupVar = "Store", NewVar= "SixWeekSalesLag",slideBy = -lag)
lag = 7*7
dlagged<- slide(dlagged, Var = "Sales", GroupVar = "Store", NewVar= "SevenWeekSalesLag",slideBy = -lag)
lag = 8*7
dlagged<- slide(dlagged, Var = "Sales", GroupVar = "Store", NewVar= "EightWeekSalesLag",slideBy = -lag)
lag = 9*7
dlagged<- slide(dlagged, Var = "Sales", GroupVar = "Store", NewVar= "NineWeekSalesLag",slideBy = -lag)
lag = 10*7
dlagged<- slide(dlagged, Var = "Sales", GroupVar = "Store", NewVar= "TenWeekSalesLag",slideBy = -lag)

data <- dlagged[dlagged$TestFlag==1,]

data <- data[,-(2:8)]

load("Datasets/TreeFinalTestData.RData")
t <- TreeFinalTestData
rm(d)
rm(dlagged)
rm(TreeFinalTestData)

d <- merge(x=t,y=data, by= "X",all.x = TRUE)

TreeFinalTestData <- d

save(TreeFinalTestData,file="Datasets/TreeFinalTestData.RData",compress = TRUE)


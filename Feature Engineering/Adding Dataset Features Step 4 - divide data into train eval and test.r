load("Datasets/CombinedData.RData")
load("Datasets/StoreSalesGroups.RData")
d<- Data
rm(Data)

d <- merge(d,StoreGroups,by="Store",all.x = TRUE)
rm(StoreGroups)

Id <- d$Id
testInd <- d$testind
SalesGroup <- d$SalesGroup

names(d)
cols.to.remove <- c("Id","testind","CompetitionOpenSinceMonth","CompetitionOpenSinceYear","AirportCode","CompetitionOpenDate",
                    "SalesMonthInterval","SalesGroup")
d <- d[,-which(names(d) %in% cols.to.remove)]

d <- cbind(SalesGroup,d)
d <- cbind(testInd,d)
d <- cbind(Id,d)
d<-d[order(d$Store,d$Date),]
d$SalesGroup <- as.factor(d$SalesGroup)

TestData <- d[d$testInd==1,]
TrainData <- d[d$testInd==0,]

TrainData$InEval <- ifelse(TrainData$Date>="2015-05-08",1,0)
EvalData <- TrainData[TrainData$InEval==1,]
TrainData <- TrainData[TrainData$InEval==0,]

names(TestData)
TestData <- TestData[,-which(names(TestData) %in% c("testInd"))]

names(TrainData)
TrainData <- TrainData[,-which(names(TrainData) %in% c("testInd","InEval"))]

names(EvalData)
EvalData <- EvalData[,-which(names(EvalData) %in% c("testInd","InEval"))]

length(names(TestData))
length(names(TrainData))
length(names(EvalData))

save(TrainData,file="Datasets/TrainData.RData",compress = TRUE)
save(EvalData,file="Datasets/EvalData.RData",compress = TRUE)
save(TestData,file="Datasets/TestData.RData",compress = TRUE)



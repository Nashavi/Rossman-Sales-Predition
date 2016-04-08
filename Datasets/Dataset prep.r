require(caret)
require(lubridate)
load("Datasets/TreeTestData.RData")

d <- TreeTestData
rm(TreeTestData)
str(d)
###d[d$Store==81,"CompetitionDistance"] <- 5382
#d <- d[,-5]
###d[sapply(d, is.character)] <- lapply(d[sapply(d, is.character)],as.factor)
d$Store <- as.factor(d$Store)
d$Date <- as.Date(d$Date,'%m/%d/%Y')
d$Open <- as.factor(d$Open)
d$Promo <- as.factor(d$Promo)
d$StateHoliday <- as.character(d$StateHoliday)
d$StateHoliday <- as.factor(ifelse(d$StateHoliday=="0","0","1"))
d$SchoolHoliday <- as.character(d$SchoolHoliday)
d$SchoolHoliday <- as.factor(ifelse(d$SchoolHoliday=="0","0","1"))
d$dayofYear <- as.numeric(strftime(d$Date, format = "%j"))
d$dayofYear <- as.integer(d$dayofYear)
d$dayofWeek <- as.integer(d$dayofWeek)
d$dayofMonth <- as.integer(d$dayofMonth)
d$weekNum <- as.integer(d$weekNum)
d$month <- as.integer(d$month)
d$quarter <- as.integer(d$quarter)
d$year <- as.integer(d$year)
d$cloudcover <- as.integer(d$cloudcover)
d$events <- as.factor(d$events)
levels(d$events)
d$events <- unclass(d$events)
d$events <- as.integer(d$events)
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

d<- d[,-which(names(d) %in% c("CompetitionOpenSinceMonth","CompetitionOpenSinceYear"))]
d<- d[,-which(names(d) %in% c("AirportCode","CompetitionOpenDate","SalesMonthInterval"))]
#d<- d[,-which(names(d) %in% c("CompetitionOpenDate","Date","Promo2StartDate","SalesMonthInterval"))]
#d<- d[,-which(names(d) %in% c("Weekday"))]
d$WeeksSinceCompOpened <- as.integer(d$WeeksSinceCompOpened)


m = matrix(data=NA,nrow=length(names(d)),ncol=2)
dimnames(m) = list(names(d),c("Total Missing","Percent Missing"))        
for(i in 1:nrow(m)){
  l = length(d[,i])
  u = length(which(is.na(d[,i])))
  m[i,"Total Missing"] = u
  m[i,"Percent Missing"] = round(u/l,digits=4)
}
m <- data.frame(m[m[,"Percent Missing"]!=0,])

load("Datasets/Train.RData")
t <- TrainData
t <- t[,-c(4,6:43)]
t$Date <- as.Date(t$Date,'%m/%d/%Y')
t <- t[t$Date>"2015-05-08",]
t$NewDate <- t$Date+ 42

d$SixWeekSalesLag <- -1
for(i in d$Id){
  s <- d[d$Id==i,"Store"]
  dt <- d[d$Id==i,"Date"]
  #print(paste(s,dt))
  l.sales <- t[t$Store==s & t$NewDate==dt,"Sales"]
  d[d$Id==i,"SixWeekSalesLag"] <- l.sales
}



TreeTestData <- d
save(TreeTestData,file="Datasets/TreeTestData.RData",compress = TRUE )


# year <- as.character(d$year)
# StateHoliday <- as.character(d$StateHoliday)
# 
# d<- d[,-which(names(d) %in% c("year","StateHoliday"))]

# d<- d[d$Open==1,]
# d$Sales <- log(d$Sales)
# 
# dmy <- dummyVars("~ .",data=d)
# d <- data.frame(predict(dmy,newdata = d))
# 
# d$year.2015 <- 1
# d$StateHoliday.0 <- 1


load("Datasets/TestData.RData")
load("Datasets/Train.RData")
load("Datasets/Eval.RData")

eval$eve


levels(TestData$weekNum) <- levels(TrainData$weekNum)
levels(TestData$Promo) <- levels(TrainData$Promo)
levels(TestData$month) <- levels(TrainData$month)
levels(TestData$quarter) <- c("1","2","3","4")
#levels(TestData$Promo2Valid) <-levels(TrainData$Promo2Valid)
TestData$events <- factor(TestData$events,levels = 
                            c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22"))



# TestData$StateHoliday_L1 <- factor(TestData$StateHoliday_L1,levels = c("0","1"),labels = c("0","1"))
# TestData$StateHoliday_L2 <- factor(TestData$StateHoliday_L2,levels = c("0","1"),labels = c("0","1"))
# TestData$StateHoliday_L3 <- factor(TestData$StateHoliday_L3,levels = c("0","1"),labels = c("0","1"))
# TestData$StateHoliday_L4 <- factor(TestData$StateHoliday_L4,levels = c("0","1"),labels = c("0","1"))
# TestData$StateHoliday_L5 <- factor(TestData$StateHoliday_L5,levels = c("0","1"),labels = c("0","1"))
# 
# 
# TestData$Promo2Valid_L1 <- factor(TestData$Promo2Valid_L1,levels = c("0","1"),labels = c("0","1"))
# TestData$Promo2Valid_L2 <- factor(TestData$Promo2Valid_L2,levels = c("0","1"),labels = c("0","1"))
# 
TestData$promovalid <- as.factor(TestData$promovalid)
# levels(TestData$promovalid) <- levels(TrainData$Promo2Valid)
# TestData$promovalid_L1 <- as.factor(TestData$promovalid_L1)
# TestData$promovalid_L2 <- as.factor(TestData$promovalid_L2)



m = matrix(data=NA,nrow=length(names(TestData)),ncol=2)
dimnames(m) = list(names(TestData),c("Total Missing","Percent Missing"))        
for(i in 1:nrow(m)){
  l = length(TestData[,i])
  u = length(which(is.na(TestData[,i])))
  m[i,"Total Missing"] = u
  m[i,"Percent Missing"] = round(u/l,digits=4)
}
m <- data.frame(m[m[,"Percent Missing"]!=0,])



FinalTestData <- TestData
save(FinalTestData,file="Datasets/FinalTestData.RData",compress = TRUE )

rm(TestData)

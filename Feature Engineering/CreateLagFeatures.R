require(DataCombine)

load("Datasets/Train.RData")
d<-TrainData
rm(TrainData)
d[d$Id==907,]

d$Store <- as.factor(d$Store)
d$Date <- as.Date(d$Date,'%m/%d/%Y')
d$Open <- factor(d$Open,levels = c("0","1"),labels = c("0","1"))
d$Promo <- factor(d$Promo,levels = c("0","1"),labels = c("0","1"))
d$StateHoliday <- as.character(ifelse(d$StateHoliday=="0","0","1"))
d$StateHoliday <- factor(d$StateHoliday,levels = c("0","1"))
d$SchoolHoliday <- as.character(ifelse(d$SchoolHoliday=="0","0","1"))
d$SchoolHoliday <- factor(d$SchoolHoliday,levels = c("0","1"))
d$dayofYear <- as.factor(d$dayofYear)
d$dayofWeek <- as.factor(d$dayofWeek)
d$dayofMonth <- as.factor(d$dayofMonth)
d$weekNum <- as.factor(d$weekNum)
d$month <- as.factor(d$month)
d$quarter <- as.factor(d$quarter)
d$year <- as.factor(d$year)
d$Promo2 <- factor(d$Promo2,levels = c("0","1"),labels = c("0","1"))
d$Promo2Valid <- factor(d$Promo2Valid,levels = c("0","1"),labels = c("0","1"))

d <- d[,-which(names(d) %in% c("DayOfWeek","Customers","dayofYear","CompetitionOpenSinceMonth","CompetitionOpenSinceYear","Promo2SinceWeek"))]
d <- d[,-which(names(d) %in% c("Promo2SinceYear","PromoInterval","StateCode","StateName","AirportCode","CompetitionOpenDate"))]
d <- d[,-which(names(d) %in% c("Promo2StartDate","SalesMonthInterval","dayOfWeek"))]

TrainData <- d

save(TrainData,file="Datasets/TrainData.RData",compress = TRUE)


d<-d[order(d$Store,d$Date),] #order by store and then dtate
d$Date <- as.Date(d$Date)

d$promovalid <- ifelse(d$Promo=="1" & d$Open=="1",1,0)
d$promovalid <- factor(d$promovalid,levels = c("0","1"),labels = c("0","1"))

#Open lead by 1
dlagged<- slide(d, Var = "Open", GroupVar = "Store", NewVar= "Open_L1", slideBy = 1)

#Promovalid lead by 1 and 2
dlagged<- slide(dlagged, Var = "promovalid", GroupVar = "Store", NewVar= "promovalid_L1", slideBy = 1)
dlagged<- slide(dlagged, Var = "promovalid_L1", GroupVar = "Store", NewVar= "promovalid_L2",slideBy = 1)

#StateHolidays lead by 1 to 5
dlagged<- slide(dlagged, Var = "StateHoliday", GroupVar = "Store", NewVar= "StateHoliday_L1", slideBy = 1)
dlagged<- slide(dlagged, Var = "StateHoliday_L1", GroupVar = "Store", NewVar= "StateHoliday_L2", slideBy = 1)
dlagged<- slide(dlagged, Var = "StateHoliday_L2", GroupVar = "Store", NewVar= "StateHoliday_L3", slideBy = 1)
dlagged<- slide(dlagged, Var = "StateHoliday_L3", GroupVar = "Store", NewVar= "StateHoliday_L4", slideBy = 1)
dlagged<- slide(dlagged, Var = "StateHoliday_L4", GroupVar = "Store", NewVar= "StateHoliday_L5", slideBy = 1)

dlagged$StateHoliday_L1<-as.factor(dlagged$StateHoliday_L1)
#levels(dlagged$StateHoliday_L1)<-c("0","a","b","c")
dlagged$StateHoliday_L2<-as.factor(dlagged$StateHoliday_L2)
#levels(dlagged$StateHoliday_L2)<-c("0","a","b","c")
dlagged$StateHoliday_L3<-as.factor(dlagged$StateHoliday_L3)
#levels(dlagged$StateHoliday_L3)<-c("0","a","b","c")
dlagged$StateHoliday_L4<-as.factor(dlagged$StateHoliday_L4)
#levels(dlagged$StateHoliday_L4)<-c("0","a","b","c")
dlagged$StateHoliday_L5<-as.factor(dlagged$StateHoliday_L5)
#levels(dlagged$StateHoliday_L5)<-c("0","a","b","c")

#SchoolHolidays lead by 1 to 5
dlagged<- slide(dlagged, Var = "SchoolHoliday", GroupVar = "Store", NewVar= "SchoolHoliday_L1", slideBy = 1)
dlagged<- slide(dlagged, Var = "SchoolHoliday_L1", GroupVar = "Store", NewVar= "SchoolHoliday_L2", slideBy = 1)
dlagged<- slide(dlagged, Var = "SchoolHoliday_L2", GroupVar = "Store", NewVar= "SchoolHoliday_L3", slideBy = 1)
dlagged<- slide(dlagged, Var = "SchoolHoliday_L3", GroupVar = "Store", NewVar= "SchoolHoliday_L4", slideBy = 1)
dlagged<- slide(dlagged, Var = "SchoolHoliday_L4", GroupVar = "Store", NewVar= "SchoolHoliday_L5", slideBy = 1)

dlagged$SchoolHoliday_L1<-as.factor(dlagged$SchoolHoliday_L1)
#levels(dlagged$SchoolHoliday_L1)<-c("0","a","b","c")
dlagged$SchoolHoliday_L2<-as.factor(dlagged$SchoolHoliday_L2)
#levels(dlagged$SchoolHoliday_L2)<-c("0","a","b","c")
dlagged$SchoolHoliday_L3<-as.factor(dlagged$SchoolHoliday_L3)
#levels(dlagged$SchoolHoliday_L3)<-c("0","a","b","c")
dlagged$SchoolHoliday_L4<-as.factor(dlagged$SchoolHoliday_L4)
#levels(dlagged$SchoolHoliday_L4)<-c("0","a","b","c")
dlagged$SchoolHoliday_L5<-as.factor(dlagged$SchoolHoliday_L5)
#levels(dlagged$SchoolHoliday_L5)<-c("0","a","b","c")

#Promo2valid lead by 1 and 2
dlagged<- slide(dlagged, Var = "Promo2Valid", GroupVar = "Store", NewVar= "Promo2Valid_L1", slideBy = 1)
dlagged<- slide(dlagged, Var = "Promo2Valid_L1", GroupVar = "Store", NewVar= "Promo2Valid_L2",slideBy = 1)

d <- dlagged

d$Open_L1 <- as.factor(d$Open_L1)
d$Promo2Valid_L1 <- as.factor(d$promovalid_L1)
d$Promo2Valid_L2 <- as.factor(d$promovalid_L2)
d$promovalid_L1 <- as.factor(d$promovalid_L1)
d$promovalid_L2 <- as.factor(d$promovalid_L2)






d[d$Id==907,]
d <- d[order(d$Id),]

TrainData <- d

save(TrainData,file="Datasets/TrainData.RData",compress = TRUE)

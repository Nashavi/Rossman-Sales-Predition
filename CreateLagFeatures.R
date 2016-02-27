require(DataCombine)

d <- read.csv("Datasets/MasterDataWithWeather.csv")

d<-d[order(d$Store,d$Date),] #order by store and then dtate
d$Date <- as.Date(d$Date)

d$promovalid <- ifelse(d$Promo=="1" & d$Open=="1",1,0)

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
levels(dlagged$StateHoliday_L1)<-c("0","a","b","c")
dlagged$StateHoliday_L2<-as.factor(dlagged$StateHoliday_L2)
levels(dlagged$StateHoliday_L2)<-c("0","a","b","c")
dlagged$StateHoliday_L3<-as.factor(dlagged$StateHoliday_L3)
levels(dlagged$StateHoliday_L3)<-c("0","a","b","c")
dlagged$StateHoliday_L4<-as.factor(dlagged$StateHoliday_L4)
levels(dlagged$StateHoliday_L4)<-c("0","a","b","c")
dlagged$StateHoliday_L5<-as.factor(dlagged$StateHoliday_L5)
levels(dlagged$StateHoliday_L5)<-c("0","a","b","c")

#SchoolHolidays lead by 1 to 5
dlagged<- slide(dlagged, Var = "SchoolHoliday", GroupVar = "Store", NewVar= "SchoolHoliday_L1", slideBy = 1)
dlagged<- slide(dlagged, Var = "SchoolHoliday_L1", GroupVar = "Store", NewVar= "SchoolHoliday_L2", slideBy = 1)
dlagged<- slide(dlagged, Var = "SchoolHoliday_L2", GroupVar = "Store", NewVar= "SchoolHoliday_L3", slideBy = 1)
dlagged<- slide(dlagged, Var = "SchoolHoliday_L3", GroupVar = "Store", NewVar= "SchoolHoliday_L4", slideBy = 1)
dlagged<- slide(dlagged, Var = "SchoolHoliday_L4", GroupVar = "Store", NewVar= "SchoolHoliday_L5", slideBy = 1)

dlagged$SchoolHoliday_L1<-as.factor(dlagged$SchoolHoliday_L1)
levels(dlagged$SchoolHoliday_L1)<-c("0","a","b","c")
dlagged$SchoolHoliday_L2<-as.factor(dlagged$SchoolHoliday_L2)
levels(dlagged$SchoolHoliday_L2)<-c("0","a","b","c")
dlagged$SchoolHoliday_L3<-as.factor(dlagged$SchoolHoliday_L3)
levels(dlagged$SchoolHoliday_L3)<-c("0","a","b","c")
dlagged$SchoolHoliday_L4<-as.factor(dlagged$SchoolHoliday_L4)
levels(dlagged$SchoolHoliday_L4)<-c("0","a","b","c")
dlagged$SchoolHoliday_L5<-as.factor(dlagged$SchoolHoliday_L5)
levels(dlagged$SchoolHoliday_L5)<-c("0","a","b","c")

#Promo2valid lead by 1 and 2
dlagged<- slide(dlagged, Var = "Promo2Valid", GroupVar = "Store", NewVar= "Promo2Valid_L1", slideBy = 1)
dlagged<- slide(dlagged, Var = "Promo2Valid_L1", GroupVar = "Store", NewVar= "Promo2Valid_L2",slideBy = 1)



require(DataCombine)

load("Datasets/Test.RData")


#load("Datasets/TreeTrainData.RData")
d<- TestData  
rm(TestData)

d<-d[order(d$Store,d$Date),] #order by store and then dtate
d$Date <- as.Date(d$Date)

d$promovalid <- ifelse(d$Promo=="1" & d$Open=="1",1,0)
d$promovalid <- factor(d$promovalid,levels = c("0","1"),labels = c("0","1"))

#Open lead by 1
dlagged <- slide(d, Var = "Open", GroupVar = "Store", NewVar= "Open_L1", slideBy = -1)

#Promovalid lead by 1 and 2
dlagged<- slide(dlagged, Var = "promovalid", GroupVar = "Store", NewVar= "promovalid_L1", slideBy = -1)
dlagged<- slide(dlagged, Var = "promovalid_L1", GroupVar = "Store", NewVar= "promovalid_L2",slideBy = -1)

#StateHolidays lead by 1 to 5
dlagged<- slide(dlagged, Var = "StateHoliday", GroupVar = "Store", NewVar= "StateHoliday_L1", slideBy = -1)
dlagged<- slide(dlagged, Var = "StateHoliday_L1", GroupVar = "Store", NewVar= "StateHoliday_L2", slideBy = -1)
dlagged<- slide(dlagged, Var = "StateHoliday_L2", GroupVar = "Store", NewVar= "StateHoliday_L3", slideBy = -1)
dlagged<- slide(dlagged, Var = "StateHoliday_L3", GroupVar = "Store", NewVar= "StateHoliday_L4", slideBy = -1)
dlagged<- slide(dlagged, Var = "StateHoliday_L4", GroupVar = "Store", NewVar= "StateHoliday_L5", slideBy = -1)

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
dlagged<- slide(dlagged, Var = "SchoolHoliday", GroupVar = "Store", NewVar= "SchoolHoliday_L1", slideBy = -1)
dlagged<- slide(dlagged, Var = "SchoolHoliday_L1", GroupVar = "Store", NewVar= "SchoolHoliday_L2", slideBy = -1)
dlagged<- slide(dlagged, Var = "SchoolHoliday_L2", GroupVar = "Store", NewVar= "SchoolHoliday_L3", slideBy = -1)
dlagged<- slide(dlagged, Var = "SchoolHoliday_L3", GroupVar = "Store", NewVar= "SchoolHoliday_L4", slideBy = -1)
dlagged<- slide(dlagged, Var = "SchoolHoliday_L4", GroupVar = "Store", NewVar= "SchoolHoliday_L5", slideBy = -1)

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
dlagged<- slide(dlagged, Var = "Promo2Valid", GroupVar = "Store", NewVar= "Promo2Valid_L1", slideBy = -1)
dlagged<- slide(dlagged, Var = "Promo2Valid_L1", GroupVar = "Store", NewVar= "Promo2Valid_L2",slideBy = -1)

#dlagged<- slide(dlagged, Var = "Sales", GroupVar = "Store", NewVar= "SixWeekSalesLag",slideBy = -42)


d <- dlagged

m = matrix(data=NA,nrow=length(names(d)),ncol=2)
dimnames(m) = list(names(d),c("Total Missing","Percent Missing"))        
for(i in 1:nrow(m)){
  l = length(d[,i])
  u = length(which(is.na(d[,i])))
  m[i,"Total Missing"] = u
  m[i,"Percent Missing"] = round(u/l,digits=4)
}
m <- data.frame(m[m[,"Percent Missing"]!=0,])

d$Open_L1 <- ifelse(is.na(d$Open_L1)==TRUE,3,d$Open_L1)
d$Open_L1 <- factor(d$Open_L1,levels=c("1","2","3"))

d$promovalid_L1 <- ifelse(is.na(d$promovalid_L1)==TRUE,3,d$promovalid_L1)
d$promovalid_L1 <- factor(d$promovalid_L1,levels=c("1","2","3"))

d$promovalid_L2 <- ifelse(is.na(d$promovalid_L2)==TRUE,3,d$promovalid_L2)
d$promovalid_L2 <- factor(d$promovalid_L2,levels=c("1","2","3"))

d$StateHoliday_L1 <- ifelse(is.na(d$StateHoliday_L1)==TRUE,"3",d$StateHoliday_L1)
d$StateHoliday_L1 <- factor(d$StateHoliday_L1,levels=c("1","2","3")) 

d$StateHoliday_L2 <- as.character(d$StateHoliday_L2)
d$StateHoliday_L2 <- ifelse(is.na(d$StateHoliday_L2)==TRUE,"3",d$StateHoliday_L2)
d$StateHoliday_L2 <- factor(d$StateHoliday_L2,levels=c("1","2","3"))

d$StateHoliday_L3 <- as.character(d$StateHoliday_L3)
d$StateHoliday_L3 <- ifelse(is.na(d$StateHoliday_L3)==TRUE,"3",d$StateHoliday_L3)
d$StateHoliday_L3 <- factor(d$StateHoliday_L3,levels=c("1","2","3"))

d$StateHoliday_L4 <- as.character(d$StateHoliday_L4)
d$StateHoliday_L4 <- ifelse(is.na(d$StateHoliday_L4)==TRUE,"3",d$StateHoliday_L4)
d$StateHoliday_L4 <- factor(d$StateHoliday_L4,levels=c("1","2","3"))

d$StateHoliday_L5 <- as.character(d$StateHoliday_L5)
d$StateHoliday_L5 <- ifelse(is.na(d$StateHoliday_L5)==TRUE,"3",d$StateHoliday_L4)
d$StateHoliday_L5 <- factor(d$StateHoliday_L5,levels=c("1","2","3"))


d$SchoolHoliday_L1 <- as.character(d$SchoolHoliday_L1)
d$SchoolHoliday_L1 <- ifelse(is.na(d$SchoolHoliday_L1)==TRUE,"3",d$SchoolHoliday_L1)
d$SchoolHoliday_L1 <- factor(d$SchoolHoliday_L1,levels=c("1","2","3")) 

d$SchoolHoliday_L2 <- as.character(d$SchoolHoliday_L2)
d$SchoolHoliday_L2 <- ifelse(is.na(d$SchoolHoliday_L2)==TRUE,"3",d$SchoolHoliday_L2)
d$SchoolHoliday_L2 <- factor(d$SchoolHoliday_L2,levels=c("1","2","3"))

d$SchoolHoliday_L3 <- as.character(d$SchoolHoliday_L3)
d$SchoolHoliday_L3 <- ifelse(is.na(d$SchoolHoliday_L3)==TRUE,"3",d$SchoolHoliday_L3)
d$SchoolHoliday_L3 <- factor(d$SchoolHoliday_L3,levels=c("1","2","3"))

d$SchoolHoliday_L4 <- as.character(d$SchoolHoliday_L4)
d$SchoolHoliday_L4 <- ifelse(is.na(d$SchoolHoliday_L4)==TRUE,"3",d$SchoolHoliday_L4)
d$SchoolHoliday_L4 <- factor(d$SchoolHoliday_L4,levels=c("1","2","3"))

d$SchoolHoliday_L5 <- as.character(d$SchoolHoliday_L5)
d$SchoolHoliday_L5 <- ifelse(is.na(d$SchoolHoliday_L5)==TRUE,"3",d$SchoolHoliday_L5)
d$SchoolHoliday_L5 <- factor(d$SchoolHoliday_L5,levels=c("1","2","3"))

d$Promo2Valid_L1 <- as.character(d$Promo2Valid_L1)
d$Promo2Valid_L1 <- ifelse(is.na(d$Promo2Valid_L1)==TRUE,"3",d$Promo2Valid_L1)
d$Promo2Valid_L1 <- factor(d$Promo2Valid_L1,levels=c("1","2","3"))

d$Promo2Valid_L2 <- as.character(d$Promo2Valid_L2)
d$Promo2Valid_L2 <- ifelse(is.na(d$Promo2Valid_L2)==TRUE,"3",d$Promo2Valid_L2)
d$Promo2Valid_L2 <- factor(d$Promo2Valid_L2,levels=c("1","2","3"))

# lag.stores <- unique(d[is.na(d$SixWeekSalesLag),"Store"])
# 
# for(i in 1:length(lag.stores)){
#   avg.sales <- mean(d[d$Store==i & d$year==2013 & d$month==1,"Sales"])
#   d[d$Store==i & is.na(d$SixWeekSalesLag)==TRUE,"SixWeekSalesLag"] <- avg.sales
#   print(paste(i,avg.sales))
# }

#d[d$Id==907,]
#d <- d[order(d$Id),]

#Competition Distance
stores <- unique(d[is.na(d$CompetitionDistance),"Store"])
for(i in stores) {
  st <- unique(d[d$Store==i,"State"])
  avgdist <- mean(d[d$State==st & is.na(d$CompetitionDistance)==FALSE,"CompetitionDistance"])
  d[d$Store==i,"CompetitionDistance"] <- avgdist
}

d$CompetitionOpenSinceYear <- as.numeric(as.character(d$CompetitionOpenSinceYear))
d$CompetitionOpenSinceMonth <- as.numeric(as.character(d$CompetitionOpenSinceMonth))
unique(d[d$Store==551,"State"])
cosy <- as.data.frame(table(d[is.na(d$CompetitionOpenSinceYear)==FALSE & d$State=="BY","CompetitionOpenSinceYear"]))
d[d$Store==551,"CompetitionOpenSinceYear"] <- 2012

stores <- unique(d[is.na(d$WeeksSinceCompOpened),"Store"])
for(i in stores) {
  st <- unique(d[d$Store==i,"State"])
  avg.yr <- round(mean(d[d$State==st & is.na(d$WeeksSinceCompOpened)==FALSE,"CompetitionOpenSinceYear"]),0)
  avg.mo <- round(mean(d[d$State==st & is.na(d$WeeksSinceCompOpened)==FALSE,"CompetitionOpenSinceMonth"]),0)
  d[d$Store==i,"CompetitionOpenSinceYear"] <- avg.yr
  d[d$Store==i,"CompetitionOpenSinceMonth"] <- avg.mo
}
d$CompetitionOpenDate <- as.Date(paste(d$CompetitionOpenSinceYear,d$CompetitionOpenSinceMonth,"01",sep = "-"))
d$WeeksSinceCompOpened <- as.integer(difftime(d$Date,d$CompetitionOpenDate,units="weeks"))
                                      
missing.rows <- d[is.na(d$cloudcover),"Id"]
d[d$Id==30935,"State"]

for(i in missing.rows){
  st <- d[d$Id==i,"State"]
  dt <- d[d$Id==i,"Date"]
  tab <- as.data.frame(table(d[d$State!=st & d$Date==dt & is.na(d$cloudcover)==FALSE,"cloudcover"]))
  cc <- which.max(tab$Freq)[1]
  d[d$Id==i,"cloudcover"] <- cc
  n <- length(d[is.na(d$cloudcover),"Id"])
  print(paste(n,"records left"))
}

d$Promo2Valid <- ifelse(is.na(d$Promo2Valid)==TRUE,"2",d$Promo2Valid)
d$Promo2Valid <- factor(d$Promo2Valid,levels=c("0","1","2"))

str(d)

d <- d[,-which(names(d) %in% c("Promo2","Promo2SinceWeek","Promo2SinceYear","PromoInterval","Promo2StartDate"))]

TreeTestData <- d

save(TreeTestData,file="Datasets/TreeTestData.RData",compress = TRUE)

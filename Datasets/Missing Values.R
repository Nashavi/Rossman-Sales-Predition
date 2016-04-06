require(DataCombine)

load("Datasets/Train.RData")
d <-TrainData
rm(TrainData)
#d[d$Id==907,]

d$Store <- as.factor(d$Store)
d$Date <- as.Date(d$Date,'%m/%d/%Y')
d$Open <- factor(d$Open,levels = c("0","1"),labels = c("0","1"))
d$Promo <- factor(d$Promo,levels = c("0","1"),labels = c("0","1"))
d$StateHoliday <- as.character(ifelse(d$StateHoliday=="0","0","1"))
d$StateHoliday <- factor(d$StateHoliday,levels = c("0","1"))
d$SchoolHoliday <- as.character(ifelse(d$SchoolHoliday=="0","0","1"))
d$SchoolHoliday <- factor(d$SchoolHoliday,levels = c("0","1"))
d$dayofYear <- as.integer(d$dayofYear)
d$dayofWeek <- as.integer(d$dayofWeek)
d$dayofMonth <- as.integer(d$dayofMonth)
d$weekNum <- as.integer(d$weekNum)
d$month <- as.integer(d$month)
d$quarter <- as.integer(d$quarter)
d$year <- as.integer(d$year)
d$Promo2 <- factor(d$Promo2,levels = c("0","1"),labels = c("0","1"))
d$Promo2Valid <- factor(d$Promo2Valid,levels = c("0","1"),labels = c("0","1"))

d$events <- as.integer(d$events)
X <- d$X  
Store <- d$Store
Date <- d$Date
Sales <- d$Sales
State <- d$StateCode
d <- d[,-which(names(d) %in% c("X","Store","Date","Sales","StateCode","StateName","AirportCode"))]
d <- d[,-c(1:2)]

d <- cbind(Sales,d)
d <- cbind(Date,d)
d<- cbind(Store,d)
d<- cbind(State,d)
d<- cbind(X,d)


m = matrix(data=NA,nrow=length(names(d)),ncol=2)
dimnames(m) = list(names(d),c("Total Missing","Percent Missing"))        
for(i in 1:nrow(m)){
  l = length(d[,i])
  u = length(which(is.na(d[,i])))
  m[i,"Total Missing"] = u
  m[i,"Percent Missing"] = round(u/l,digits=4)
}
m <- data.frame(m[m[,"Percent Missing"]!=0,])

#### impute missing competition distance using avg value from other stores in the same state
missing.stores <- unique(d[is.na(d$CompetitionDistance),"Store"])
for(i in missing.stores) {
  x <- i 
  st <- unique(d[d$Store==x,"State"])
  avg.dist <- mean(d[d$State==st & is.na(d$CompetitionDistance)==FALSE,"CompetitionDistance"])
  d[d$Store==i,"CompetitionDistance"] <- avg.dist
}

#### impute missing weeks since competition opened
d$CompetitionOpenDate <- as.Date(d$CompetitionOpenDate,'%m/%d/%Y')
### one store has an open year of 1900 -  Rossman didn't open until 1970
bad.rec <- unique(d[which.min(d$CompetitionOpenDate),"Store"])
unique(d[d$Store==bad.rec,"CompetitionOpenSinceYear"])
d[d$Store==bad.rec,"CompetitionOpenSinceYear"] <- 1970
d[d$Store==bad.rec,"WeeksSinceCompOpened"] <- NA
d[d$Store==bad.rec,"WeeksSinceCompOpened"]


missing.stores <- unique(d[is.na(d$WeeksSinceCompOpened),"Store"])
missing.stores
for(i in missing.stores) {
  x <- i 
  st <- unique(d[d$Store==x,"State"])
  avg.mo <- round(mean(d[d$State==st & is.na(d$CompetitionOpenSinceMonth)==FALSE,"CompetitionOpenSinceMonth"]),0)
  avg.yr <- round(mean(d[d$State==st & is.na(d$CompetitionOpenSinceYear)==FALSE,"CompetitionOpenSinceYear"]),0)
  #print(paste(i,st,avg.mo,avg.yr))
  d[d$Store==i,"CompetitionOpenSinceMonth"] <- avg.mo
  d[d$Store==i,"CompetitionOpenSinceYear"] <- avg.yr
  d[d$Store==i,"CompetitionOpenDate"] <- as.Date(paste(avg.yr,avg.mo,"01",sep = "-"))
  
}
d$WeeksSinceCompOpened <- ifelse(is.na(d$WeeksSinceCompOpened)==TRUE,as.integer(difftime(d$Date,d$CompetitionOpenDate,units="weeks")),
                                 d$WeeksSinceCompOpened)
d<- d[,-which(names(d) %in% c("CompetitionOpenSinceMonth","CompetitionOpenSinceYear","CompetitionOpenDate"))]

#### deal with promo2 issues - basically there is no way ok knowing if it was offered so add a level indicating it was missing
d$Promo2Valid <- as.character(d$Promo2Valid)
d$Promo2Valid <- ifelse(is.na(d$Promo2Valid)==TRUE,"2",d$Promo2Valid)
d$Promo2Valid <- factor(d$Promo2Valid,levels=c("0","1","2"))

d <- d[,-which(names(d) %in% c("Promo2","Promo2SinceWeek","Promo2SinceYear","PromoInterval","Promo2StartDate","SalesMonthInterval"))]

?table
#### impute missing events
missing.rows <- unique(d[is.na(d$events),"X"])
missing.rows
d[d$X==363393,]

for(i in missing.rows) {
  x <- i
  dt <- d[d$X==x,"Date"]
  st <- unique(d[d$X==x,"State"])
  counts <- data.frame(table(d[d$State==st & d$Date==dt & is.na(d$events)==FALSE,"events"]))
  et <- counts[which.max(counts$Freq),"Var1"]
  #print(paste(i,dt,st,et))
  d[d$X==x,"events"] <- et
  missing <- length(d[is.na(d$events),"X"])
  print(paste(missing,"records to go..."))
}


#### impute missing cloud cover
missing.rows <- unique(d[is.na(d$cloudcover),"X"])
missing.rows
for(i in missing.rows) {
  x <- i
  dt <- d[d$X==x,"Date"]
  st <- unique(d[d$X==x,"State"])
  counts <- data.frame(table(d[d$State==st & d$Date==dt & is.na(d$cloudcover)==FALSE,"cloudcover"]))
  cc <- counts[which.max(counts$Freq),"Var1"]
  #print(paste(i,dt,st,et))
  d[d$X==x,"cloudcover"] <- cc
  missing <- length(d[is.na(d$cloudcover),"X"])
  print(paste(missing,"records to go..."))
}

TreeTrainData <- d

save(TreeTrainData,file="Datasets/TreeTrainData.RData",compress = TRUE)
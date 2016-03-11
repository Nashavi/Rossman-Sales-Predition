require(caret)
load("Datasets/GrandMasterData.RData")

GrandMasterData

d <- TestData
rm(TestData)
str(d)
d[d$Id==907,]

d[sapply(d, is.character)] <- lapply(d[sapply(d, is.character)],as.factor)
# d$Store <- as.factor(d$Store)
# d$Date <- as.Date(d$Date)
# d$Open <- as.factor(d$Open)
# d$Promo <- as.factor(d$Promo)
# d$StateHoliday <- as.character(d$StateHoliday)
# d$StateHoliday <- as.factor(ifelse(d$StateHoliday=="0","0","1"))
# d$SchoolHoliday <- as.character(d$SchoolHoliday)
# d$SchoolHoliday <- as.factor(ifelse(d$SchoolHoliday=="0","0","1"))
# #d$dayofYear <- as.factor(d$dayofYear)
# d$dayofWeek <- as.factor(d$dayofWeek)
# d$dayofMonth <- as.factor(d$dayofMonth)
# d$weekNum <- as.factor(d$weekNum)
# d$month <- as.factor(d$month)
# d$quarter <- as.factor(d$quarter)
# d$year <- as.factor(d$year)
# d$CompetitionOpenSinceMonth <- as.factor(d$CompetitionOpenSinceMonth)
# d$CompetitionOpenSinceYear <- as.factor(d$CompetitionOpenSinceYear)
# d$Promo2 <- as.factor(d$Promo2)
# d$Promo2Valid <- as.factor(d$Promo2Valid)
# d$promovalid <- as.factor(d$promovalid)
d$Open_L1 <- as.factor(d$Open_L1)
d$promovalid_L1 <- as.factor(d$promovalid_L1)
d$promovalid_L2 <- as.factor(d$promovalid_L2)
d$Promo2Valid_L1 <- as.factor(d$Promo2Valid_L1)
d$Promo2Valid_L2 <- as.factor(d$Promo2Valid_L2)

d<- d[,-which(names(d) %in% c("CompetitionOpenSinceMonth","CompetitionOpenSinceYear"))]
d<- d[,-which(names(d) %in% c("Promo2","Promo2SinceWeek","Promo2SinceYear","PromoInterval"))]
d<- d[,-which(names(d) %in% c("CompetitionOpenDate","Date","Promo2StartDate","SalesMonthInterval"))]
d<- d[,-which(names(d) %in% c("Name","Customers","AirportCode"))]
d$WeeksSinceCompOpened <- as.integer(d$WeeksSinceCompOpened)


d[d$Id==907,]

m = matrix(data=NA,nrow=length(names(d)),ncol=2)
dimnames(m) = list(names(d),c("Total Missing","Percent Missing"))        
for(i in 1:nrow(m)){
  l = length(d[,i])
  u = length(which(is.na(d[,i])))
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

d$CompetitionDistance <- ifelse(is.na(d$CompetitionDistance)==TRUE,
                                random.imp(d$CompetitionDistance),
                                d$CompetitionDistance)
d$WeeksSinceCompOpened <- ifelse(is.na(d$WeeksSinceCompOpened)==TRUE,
                                 random.imp(d$WeeksSinceCompOpened),
                                 d$WeeksSinceCompOpened)

d$Promo2Valid <- as.character(d$Promo2Valid)
d$Promo2Valid <- as.factor(ifelse(is.na(d$Promo2Valid)==TRUE,"2",d$Promo2Valid))

d$cloudcover <- as.factor(ifelse(is.na(d$cloudcover)==TRUE,
                                 random.imp(d$cloudcover),
                                 d$cloudcover))
d$events <- as.factor(ifelse(is.na(d$events)==TRUE,
                             random.imp(d$events),
                             d$events))

d$Open_L1 <- as.character(d$Open_L1)
d$Open_L1 <- as.factor(ifelse(is.na(d$Open_L1)==TRUE,"2",d$Open_L1))
d$promovalid_L1 <- as.character(d$promovalid_L1)
d$promovalid_L1 <- as.factor(ifelse(is.na(d$promovalid_L1)==TRUE,"2",d$promovalid_L1))
d$promovalid_L2 <- as.character(d$promovalid_L2)
d$promovalid_L2 <- as.factor(ifelse(is.na(d$promovalid_L2)==TRUE,"2",d$promovalid_L2))
d$StateHoliday_L1 <- as.character(d$StateHoliday_L1)
d$StateHoliday_L1 <- as.factor(ifelse(is.na(d$StateHoliday_L1)==TRUE,"2",d$StateHoliday_L1))
d$StateHoliday_L2 <- as.character(d$StateHoliday_L2)
d$StateHoliday_L2 <- as.factor(ifelse(is.na(d$StateHoliday_L2)==TRUE,"2",d$StateHoliday_L2))
d$StateHoliday_L3 <- as.character(d$StateHoliday_L3)
d$StateHoliday_L3 <- as.factor(ifelse(is.na(d$StateHoliday_L3)==TRUE,"2",d$StateHoliday_L3))
d$StateHoliday_L4 <- as.character(d$StateHoliday_L4)
d$StateHoliday_L4 <- as.factor(ifelse(is.na(d$StateHoliday_L4)==TRUE,"2",d$StateHoliday_L4))
d$StateHoliday_L5 <- as.character(d$StateHoliday_L5)
d$StateHoliday_L5 <- as.factor(ifelse(is.na(d$StateHoliday_L5)==TRUE,"2",d$StateHoliday_L5))
d$SchoolHoliday_L1 <- as.character(d$SchoolHoliday_L1)
d$SchoolHoliday_L1 <- as.factor(ifelse(is.na(d$SchoolHoliday_L1)==TRUE,"2",d$SchoolHoliday_L1))
d$SchoolHoliday_L2 <- as.character(d$SchoolHoliday_L2)
d$SchoolHoliday_L2 <- as.factor(ifelse(is.na(d$SchoolHoliday_L2)==TRUE,"2",d$SchoolHoliday_L2))
d$SchoolHoliday_L3 <- as.character(d$SchoolHoliday_L3)
d$SchoolHoliday_L3 <- as.factor(ifelse(is.na(d$SchoolHoliday_L3)==TRUE,"2",d$SchoolHoliday_L3))
d$SchoolHoliday_L4 <- as.character(d$SchoolHoliday_L4)
d$SchoolHoliday_L4 <- as.factor(ifelse(is.na(d$SchoolHoliday_L4)==TRUE,"2",d$SchoolHoliday_L4))
d$SchoolHoliday_L5 <- as.character(d$SchoolHoliday_L5)
d$SchoolHoliday_L5 <- as.factor(ifelse(is.na(d$SchoolHoliday_L5)==TRUE,"2",d$SchoolHoliday_L5))
d$Promo2Valid_L1 <- as.character(d$Promo2Valid_L1)
d$Promo2Valid_L1 <- as.factor(ifelse(is.na(d$Promo2Valid_L1)==TRUE,"2",d$Promo2Valid_L1))
d$Promo2Valid_L2 <- as.character(d$Promo2Valid_L2)
d$Promo2Valid_L2 <- as.factor(ifelse(is.na(d$Promo2Valid_L2)==TRUE,"2",d$Promo2Valid_L2))

rm(m)
rm(i)
rm(l)
rm(u)

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

FinalTestData <- d

save(FinalTestData,file="Datasets/FinalTestData.RData",compress = TRUE )


require(caret)
load("Datasets/TrainData.RData")
#load("Datasets/Train.RData")

d <- TrainData
rm(TrainData)
str(d)
###d[d$Store==81,"CompetitionDistance"] <- 5382

###d[sapply(d, is.character)] <- lapply(d[sapply(d, is.character)],as.factor)
# d$Store <- as.factor(d$Store)
# d$Date <- as.Date(d$Date)
# d$Open <- as.factor(d$Open)
# d$Promo <- as.factor(d$Promo)
# d$StateHoliday <- as.character(d$StateHoliday)
# d$StateHoliday <- as.factor(ifelse(d$StateHoliday=="0","0","1"))
# d$SchoolHoliday <- as.character(d$SchoolHoliday)
# d$SchoolHoliday <- as.factor(ifelse(d$SchoolHoliday=="0","0","1"))
d$dayofYear <- as.integer(d$dayofYear)
d$dayofWeek <- as.integer(d$dayofWeek)
d$dayofMonth <- as.integer(d$dayofMonth)
d$weekNum <- as.integer(d$weekNum)
d$month <- as.integer(d$month)
d$quarter <- as.integer(d$quarter)
d$year <- as.integer(d$year)
d$events <- as.integer(d$events)

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
d<- d[,-which(names(d) %in% c("Weekday"))]
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
d$Promo2Valid <- ifelse(is.na(d$Promo2Valid)==TRUE,"2",d$Promo2Valid)
d$Promo2Valid <- factor(d$Promo2Valid,levels=c("0","1","2"))

d$cloudcover <- as.factor(ifelse(is.na(d$cloudcover)==TRUE,
                                 random.imp(d$cloudcover),
                                 d$cloudcover))
d$events <- as.factor(ifelse(is.na(d$events)==TRUE,
                             random.imp(d$events),
                             d$events))

d$Open_L1 <- as.character(d$Open_L1)
d$Open_L1 <-ifelse(is.na(d$Open_L1)==TRUE,"3",d$Open_L1)
d$Open_L1 <- factor(d$Open_L1,levels=c("1","2","3"))

d$promovalid_L1 <- as.character(d$promovalid_L1)
d$promovalid_L1 <- ifelse(is.na(d$promovalid_L1)==TRUE,"3",d$promovalid_L1)
d$promovalid_L1 <- factor(d$promovalid_L1,levels=c("1","2","3")) 

d$promovalid_L2 <- as.character(d$promovalid_L2)
d$promovalid_L2 <- ifelse(is.na(d$promovalid_L2)==TRUE,"3",d$promovalid_L2)
d$promovalid_L2 <- factor(d$promovalid_L2,levels=c("1","2","3")) 

d$StateHoliday_L1 <- as.character(d$StateHoliday_L1)
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


rm(m)
rm(i)
rm(l)
rm(u)


TrainData <- d
save(TrainData,file="Datasets/TrainData.RData",compress = TRUE )


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

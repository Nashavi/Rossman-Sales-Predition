d <- read.csv("MasterDataSet.csv")
m = matrix(data=NA,nrow=length(names(d)),ncol=2)
dimnames(m) = list(names(d),c("Total Missing","Percent Missing"))        
for(i in 1:nrow(m)){
  l = length(d[,i])
  u = length(which(is.na(d[,i])))
  m[i,"Total Missing"] = u
  m[i,"Percent Missing"] = round(u/l,digits=4)
}
m[m[,"Percent Missing"]!=0,]


nocompdist <- d[is.na(d$CompetitionDistance),]
table(nocompdist$Store)
nocompdist <- nocompdist[c(-1,-4)]
##d[d$Store==957 & is.na(d$CompetitionDistance)==FALSE,]
attach(nocompdist)

require(ggplot2)

#can we model competition distance 
#- it's fixed so we should be able to figure it out from the other 997 stores!
# really nothing good for linear modeling - maybe we could cluster the stores based on attributes
# then hotdeck inpute?
t <- d[is.na(d$CompetitionDistance)==FALSE,]
t <- t[c(-1,-4)]
attach(t)

t$Store <- as.factor(Store)
t$Date <- as.Date(Date)
t$Open <- as.factor(Open)
t$Promo <- as.factor(Promo)
t$StateHoliday <- as.factor(StateHoliday)
t$SchoolHoliday <- as.factor(StateHoliday)
t$dayofYear <- as.factor(dayofYear)
t$dayofWeek <- as.factor(dayofWeek)
t$weekday <- as.factor(weekday)
t$dayofMonth <- as.factor(dayofMonth)
t$weekNum <- as.factor(weekNum)
t$month <- as.factor(month)
t$quarter <- as.factor(quarter)
t$year <- as.factor(year)
t$CompetitionOpenSinceMonth <- as.factor(CompetitionOpenSinceMonth)
t$CompetitionOpenSinceYear <- as.factor(CompetitionOpenSinceYear)
t$Promo2 <- as.factor(Promo2)
t$Promo2SinceWeek <- as.factor(Promo2SinceWeek)
t$Promo2SinceYear <- as.factor(Promo2SinceYear)
t$Promo2Valid <- as.factor(Promo2Valid)

options(contrasts = rep ("contr.treatment", 2))
attach(t)
#full.model = lm(CompetitionDistance~.,data=t)

summary(lm(CompetitionDistance~CompetitionOpenSinceYear,data=t))

null.model = lm(CompetitionDistance~1,data=t)
summary(null.model)
#step.model = step(null.model,scope=list(upper=full.model),data=train,direction="both")




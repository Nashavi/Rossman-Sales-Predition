#read in the data"
#sales <- read.csv("C:/Users/Kyle/OneDrive/UCD/Analytics/Practicum/practicum/train.csv")
stores <- read.csv("C:/Users/Kyle/OneDrive/UCD/Analytics/Practicum/practicum/stores.csv")

m = matrix(data=NA,nrow=length(names(stores)),ncol=2)
dimnames(m) = list(names(stores),c("Total Missing","Percent Missing"))        
for(i in 1:nrow(m)){
  l = length(stores[,i])
  u = length(which(is.na(stores[,i])))
  m[i,"Total Missing"] = u
  m[i,"Percent Missing"] = round(u/l,digits=4)
}
m[m[,"Percent Missing"]!=0,]

attach(stores)
require(ggplot2)

#barplot(table(CompetitionDistance,useNA = "ifany"),main="Missing Distance")
barplot(table(CompetitionOpenSinceMonth,useNA = "ifany"),main="Missing Months")
barplot(table(CompetitionOpenSinceYear,useNA = "ifany"),main="Missing Years")
barplot(table(Promo2SinceWeek,useNA = "ifany"),main="Promo 2 Week")
barplot(table(Promo2SinceYear,useNA = "ifany"),main="Promo 2 Year")

stores_misingCOSM <- stores[is.na(stores$CompetitionOpenSinceMonth)=="TRUE",]
stores_NotmisingCOSM <- stores[is.na(stores$CompetitionOpenSinceMonth)=="FALSE",]

#missing comp open since month
par(mfrow=c(1,2))
#hist(stores_misingCOSM$Store)
#hist(stores_NotmisingCOSM$Store)
barplot(table(stores_misingCOSM$Assortment))
barplot(table(stores_NotmisingCOSM$Assortment))
barplot(table(stores_misingCOSM$StoreType))
barplot(table(stores_NotmisingCOSM$StoreType))
hist(stores_misingCOSM$CompetitionDistance)
hist(stores_NotmisingCOSM$CompetitionDistance)

stores_misingCOSY <- stores[is.na(stores$CompetitionOpenSinceYear)=="TRUE",]
stores_NotmisingCOSY <- stores[is.na(stores$CompetitionOpenSinceYear)=="FALSE",]

#missing comp open since month
par(mfrow=c(1,2))
hist(stores_misingCOSY$Store)
hist(stores_NotmisingCOSY$Store)
barplot(table(stores_misingCOSY$Assortment))
barplot(table(stores_NotmisingCOSY$Assortment))
barplot(table(stores_misingCOSY$StoreType))
barplot(table(stores_NotmisingCOSY$StoreType))
hist(stores_misingCOSY$CompetitionDistance)
hist(stores_NotmisingCOSY$CompetitionDistance)


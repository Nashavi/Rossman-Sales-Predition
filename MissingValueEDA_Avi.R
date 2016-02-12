store<-read.csv("~/Documents/UCD/BA Prac/rossman_data/stores.csv")

barplot(table(store$CompetitionOpenSinceMonth,useNA = "ifany"),main="Missing Months")
barplot(table(store$CompetitionOpenSinceYear,useNA = "ifany"),main="Missing Years")
barplot(table(store$Promo2SinceWeek,useNA = "ifany"),main="Promo 2 Week")
barplot(table(store$Promo2SinceYear,useNA = "ifany"),main="Promo 2 Year")

store_COSM_NA<-store[is.na(store$CompetitionOpenSinceMonth)=="TRUE",] #Store file with COSM NA
store_COSM_noNA<-store[is.na(store$CompetitionOpenSinceMonth)=="FALSE",] #Store file with COSM noNA
par(mfrow=c(1,2)) #Plots to check if the missing is at random for COSM
barplot(table(store_COSM_NA$Assortment))
barplot(table(store_COSM_noNA$Assortment))
barplot(table(store_COSM_NA$StoreType))
barplot(table(store_COSM_noNA$StoreType))
hist(store_COSM_NA$CompetitionDistance)
hist(store_COSM_noNA$CompetitionDistance)

store_COSY_NA<-store[is.na(store$CompetitionOpenSinceYear)=="TRUE",] #Store file with COSY NA
store_COSY_noNA<-store[is.na(store$CompetitionOpenSinceYear)=="FALSE",] #Store file with COSY noNA
barplot(table(store_COSY_NA$Assortment)) #Plots to check if the missing is at random for COSM
barplot(table(store_COSY_noNA$Assortment))
barplot(table(store_COSY_NA$StoreType))
barplot(table(store_COSY_noNA$StoreType))
hist(store_COSY_NA$CompetitionDistance)
hist(store_COSY_noNA$CompetitionDistance)

#Conclusion: No patterns identified for stores missing values vs non missing values. ALl missing values are at random.

dev.off()

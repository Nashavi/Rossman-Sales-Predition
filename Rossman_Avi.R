require(lubridate)
require(ggplot2)
require(splitstackshape)
require(plyr)

rm(list=ls())
sales=read.csv("~/Documents/UCD/BA Prac/rossman_data/train.csv")
store=read.csv("~/Documents/UCD/BA Prac/rossman_data/stores.csv")
d<-merge(sales,store,by="Store",all.x=TRUE)
colSums(is.na(d))

#Split Date to year, month and day columns
d$Year<-as.numeric(substr(dataset$Date,1,4))
d$Month<-as.numeric(substr(dataset$Date,6,7))
dataset$Day<-as.numeric(substr(dataset$Date,9,10))

#Competition Feature
d$CompetitionOpenDate <- paste(d$CompetitionOpenSinceYear,d$CompetitionOpenSinceMonth,"01", sep="-")
d$datetest <- difftime(strptime(d$Date,format="%Y-%m-%d"),strptime(d$CompetitionOpenDate,format="%Y-%m-%d"),units="weeks")


#EDA
str(d)
d$Store<-as.factor(d$Store)
d$DayOfWeek<-as.factor(d$DayOfWeek)
d$Open<-as.factor(d$Open)
d$Promo<-as.factor(d$Promo)
d$SchoolHoliday<-as.factor(d$SchoolHoliday)
d$datetest<-as.integer(d$datetest)

#Missing Values & EDA plot from just the features in Stores file
colSums(is.na(store)) #Number of missing values

#Univariate Plots
barplot(table(d$DayOfWeek), main="Day of Week", las=1)
hist(d$Customers)
barplot(table(d$Open))
barplot(table(d$Promo))
barplot(table(d$StateHoliday))
barplot(table(d$SchoolHoliday))
barplot(table(d$StoreType))
barplot(table(d$Assortment))
hist(d$CompetitionDistance)
hist(d$datetest)

#Bivariate Plots
ggplot(d,aes(factor(Store),Sales))+geom_boxplot()
ggplot(d,aes(factor(Open),Sales))+geom_boxplot()
ggplot(d,aes(factor(Promo),Sales))+geom_boxplot()
ggplot(d,aes(factor(StateHoliday),Sales))+geom_boxplot()
ggplot(d,aes(factor(SchoolHoliday),Sales))+geom_boxplot()
ggplot(d,aes(factor(Promo),Sales))+geom_boxplot()
ggplot(d,aes(factor(Assortment),Sales))+geom_boxplot()
ggplot(d,aes(factor(StoreType),Sales))+geom_boxplot()
ggplot(d,aes(factor(Promo),Sales))+geom_boxplot()
qplot(d$CompetitionDistance,d$Sales)
qplot(d$datetest,d$Sales)
qplot(d$Customers,d$Sales)

#Promo2 feature engineering
da<-concat.split.multiple(d,"PromoInterval",",")
str(da$PromoInterval_1)
levels(da$PromoInterval_1) <- c("02","01","03")
str(da$PromoInterval_2)
levels(da$PromoInterval_2) <- c("04","06","05")
str(da$PromoInterval_3)
levels(da$PromoInterval_3) <- c("08","07","09")
str(da$PromoInterval_4)
levels(da$PromoInterval_4) <- c("12","11","10")


da$salemonth<-substr(da$Date,6,7)
da$Promo2exists<-0
da$Promo2exists<- ifelse(da$salemonth==da$PromoInterval_1 | da$salemonth==da$PromoInterval_2 | da$salemonth==da$PromoInterval_3| da$salemonth==da$PromoInterval_4,1,0)


table(store$Promo2SinceWeek)
summary(da$Date)

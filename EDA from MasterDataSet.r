
d <- read.csv("MasterDataSet.csv")
attach(d)
d <- d[-1]
d$Store <- as.factor(Store)
d$Date <- as.Date(Date)
d$Open <- as.factor(Open)
d$Promo <- as.factor(Promo)
d$StateHoliday <- as.factor(StateHoliday)
d$SchoolHoliday <- as.factor(StateHoliday)
d$dayofYear <- as.factor(dayofYear)
d$dayofWeek <- as.factor(dayofWeek)
d$weekday <- as.factor(weekday)
d$dayofMonth <- as.factor(dayofMonth)
d$weekNum <- as.factor(weekNum)
d$month <- as.factor(month)
d$quarter <- as.factor(quarter)
d$year <- as.factor(year)
d$CompetitionOpenSinceMonth <- as.factor(CompetitionOpenSinceMonth)
d$CompetitionOpenSinceYear <- as.factor(CompetitionOpenSinceYear)
d$Promo2 <- as.factor(Promo2)
d$Promo2SinceWeek <- as.factor(Promo2SinceWeek)
d$Promo2SinceYear <- as.factor(Promo2SinceYear)
d$Promo2Valid <- as.factor(Promo2Valid)

str(d)


require(ggplot2)

barplot(table(dayofWeek),main="Day Of Week",las=2)
hist(Customers,col="grey",main="Customers")
barplot(table(Open),main="Open",las=2)
barplot(table(Promo),main="Promo",las=2)
barplot(table(StateHoliday),main="State Holiday",las=2)
barplot(table(SchoolHoliday),main="School Holiday",las=2)
barplot(table(StoreType),main="Store Type",las=2)
barplot(table(Assortment),main="Assortment",las=2)
hist(CompetitionDistance,col="grey",main="Comp Distance")


attach(d)
bp = geom_boxplot() #assign the geom to a variable to simplify code
noleg = theme(legend.position="none")

ggplot(d,aes(factor(dayofWeek),Sales,fill=dayofWeek))+bp+ggtitle("Sales by Day of Week")+noleg
qplot(Customers,Sales,d,main="Sales by Total Customers")
ggplot(d,aes(factor(Open),Sales,fill=Open))+bp+ggtitle("Sales by Open")
ggplot(d,aes(factor(Promo),Sales,fill=Promo))+bp+ggtitle("Sales by Promo")
ggplot(d,aes(factor(StateHoliday),Sales,fill=StateHoliday))+bp+ggtitle("Sales by State Holiday")
ggplot(d,aes(factor(SchoolHoliday),Sales,fill=SchoolHoliday))+bp+ggtitle("Sales by School Holiday")
ggplot(d,aes(factor(StoreType),Sales,fill=StoreType))+bp+ggtitle("Sales by Store Type")
ggplot(d,aes(factor(Assortment),Sales,fill=Assortment))+bp+ggtitle("Sales by Assortment")
qplot(CompetitionDistance,Sales,d,main="Sales by Competition Distance")
qplot(daysdiff,Sales,d,main="Sales by DateDiff")
ggplot(d,aes(factor(Promo2Valid),Sales,fill=Promo2Valid))+bp+ggtitle("Sales by Promo2")

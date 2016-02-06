
#read in the data"
sales <- read.csv("train.csv")
stores <- read.csv("stores.csv")

#derive promo2 month/days from promo2 week
stores$Promo2SinceMonth <- ceiling(stores$Promo2SinceWeek /52*12)
stores$Promo2SinceDay <- (stores$Promo2SinceWeek * 7) - 6 #default to the start of the week

#combine promo2 month and year into a default promo2 start date - THIS ISNT 100% accurate since we're assuming 
# the date is the first of the month instead of mapping to week number
stores$Promo2StartDate <- as.Date(ifelse(
  is.na(stores$Promo2SinceYear),NA,
  paste(stores$Promo2SinceYear,stores$Promo2SinceMonth,"01",sep = "-")
))

#what are the different values for promo2 intervals?
levels(stores$PromoInterval)
#flag the valid promo2 month based on the interval (this could be done in a loop but it was easier to 
#just explictly assign the values)
stores$P2M1 <- as.integer(ifelse(stores$PromoInterval=="Jan,Apr,Jul,Oct",1,
                                 ifelse(stores$PromoInterval=="Feb,May,Aug,Nov",2,
                                        ifelse(stores$PromoInterval=="Mar,Jun,Sept,Dec",3,""))))
stores$P2M2 <- as.integer(ifelse(stores$PromoInterval=="Jan,Apr,Jul,Oct",4,
                                 ifelse(stores$PromoInterval=="Feb,May,Aug,Nov",5,
                                        ifelse(stores$PromoInterval=="Mar,Jun,Sept,Dec",6,""))))
stores$P2M3 <- as.integer(ifelse(stores$PromoInterval=="Jan,Apr,Jul,Oct",7,
                                 ifelse(stores$PromoInterval=="Feb,May,Aug,Nov",8,
                                        ifelse(stores$PromoInterval=="Mar,Jun,Sept,Dec",9,""))))
stores$P2M4 <- as.integer(ifelse(stores$PromoInterval=="Jan,Apr,Jul,Oct",10,
                                 ifelse(stores$PromoInterval=="Feb,May,Aug,Nov",11,
                                        ifelse(stores$PromoInterval=="Mar,Jun,Sept,Dec",12,""))))

# what date did the comp open?
stores$CompetitionOpenDate <- as.Date(ifelse(
  is.na(stores$CompetitionOpenSinceYear),NA,
  paste(stores$CompetitionOpenSinceYear,stores$CompetitionOpenSinceMonth,"01",sep = "-")
))


#combine the data
d <- merge(x=sales,y=stores, by= "Store",all.x = TRUE)
d$Promo2StartDate = as.Date(Promo2StartDate)




#split out some of the date parts for the sale date
require(lubridate)
d$Date = as.Date(d$Date,format="%Y-%m-%d")
d$SaleYear <- year(d$Date)
d$SaleMonth <- month(d$Date)
d$SaleWeek <- week(d$Date)

#d$CompetitionOpenDays
as.period(interval(d$CompetitionOpenDate,d$Date,units="day")

d$CompetitionDaysOpen <- as.integer(difftime(d$Date,d$CompetitionOpenDate,units="days"))

d$CompetitionWeeksOpen <- as.integer(difftime(d$Date,d$CompetitionOpenDate,units="weeks"))

d$CompetitionMonthsOpen <- as.integer(difftime(d$Date,d$CompetitionOpenDate,units="days")*365/52)

# determine if Promo2 was offered during the sale date:
d$IsPromo2Valid <- ifelse(
  d$Date >= d$Promo2StartDate
  & d$SaleMonth == d$P2M1 | d$SaleMonth == d$P2M2 | d$SaleMonth == d$P2M3 | d$SaleMonth == d$P2M4
  ,1,0
)



attach(d)
#par(mfrow=c(1,1)) #change output to 2X2
#barplot(table(Store),main="Store",las=2)
barplot(table(DayOfWeek),main="Day Of Week",las=2)
barplot(table(SaleWeek),main="Sale Week",las=2)
barplot(table(SaleMonth),main="Sale Month",las=2)
barplot(table(SaleYear),main="Sale Year",las=2)

hist(Customers,col="grey",main="Customers")
barplot(table(Open),main="Open",las=2)
barplot(table(Promo),main="Promo",las=2)
barplot(table(StateHoliday),main="State Holiday",las=2)
barplot(table(SchoolHoliday),main="School Holiday",las=2)
barplot(table(StoreType),main="Store Type",las=2)
barplot(table(Assortment),main="Assortment",las=2)
hist(CompetitionDistance,col="grey",main="Comp Distance")
hist(CompetitionDaysOpen,col="grey",main="Comp Days Open")


attach(d)
require(ggplot2)
bp = geom_boxplot() #assign the geom to a variable to simplify code
noleg = theme(legend.position="none")

#ggplot(d,aes(factor(Store),Sales))+bp+ggtitle("Sales by Store")

ggplot(d,aes(factor(DayOfWeek),Sales,fill=DayOfWeek))+bp+ggtitle("Sales by Day of Week")+noleg
ggplot(d,aes(factor(SaleWeek),Sales,fill=SaleWeek))+bp+ggtitle("Sales by Week")+noleg
ggplot(d,aes(factor(SaleMonth),Sales,fill=SaleMonth))+bp+ggtitle("Sales by Month")+noleg
ggplot(d,aes(factor(SaleYear),Sales,fill=SaleYear))+bp+ggtitle("Sales by Year")+noleg
qplot(Customers,Sales,d, ,main="Sales by Total Customers")
ggplot(d,aes(factor(Open),Sales,fill=Open))+bp+ggtitle("Sales by Open")
ggplot(d,aes(factor(Promo),Sales,fill=Promo))+bp+ggtitle("Sales by Promo")
ggplot(d,aes(factor(StateHoliday),Sales,fill=StateHoliday))+bp+ggtitle("Sales by State Holiday")
ggplot(d,aes(factor(SchoolHoliday),Sales,fill=SchoolHoliday))+bp+ggtitle("Sales by School Holiday")
ggplot(d,aes(factor(StoreType),Sales,fill=StoreType))+bp+ggtitle("Sales by Store Type")
ggplot(d,aes(factor(Assortment),Sales,fill=Assortment))+bp+ggtitle("Sales by Assortment")
qplot(CompetitionDistance,Sales,d,main="Sales by Competition Distance") #curvature???
qplot(CompetitionOpenSinceMonth,Sales,d,main="Sales by Competition Open Month")
ggplot(d,aes(factor(IsPromo2Valid),Sales,fill=IsPromo2Valid))+bp+noleg
qplot(CompetitionDaysOpen,Sales,d,main="Sales by Competition Days Open")
qplot(CompetitionWeeksOpen,Sales,d,main="Sales by Competition Weeks Open")  
qplot(CompetitionMonthsOpen,Sales,d,main="Sales by Competition Months Open") 

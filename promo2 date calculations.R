#read in the data"
sales <- read.csv("C:/Users/Kyle/OneDrive/UCD/Analytics/Practicum/practicum/train.csv")
stores <- read.csv("C:/Users/Kyle/OneDrive/UCD/Analytics/Practicum/practicum/stores.csv")

#derive promo2 month/days from promo2 week
stores$Promo2SinceMonth <- ceiling(stores$Promo2SinceWeek /52*12)
stores$Promo2SinceDay <- (stores$Promo2SinceWeek * 7) - 6 #default to the start of the week

#combine promo2 month and year into a default promo2 start date - THIS ISNT 100% accurate since we're assuming 
# the date is the first of the month instead of mapping to week number
stores$Promo2StartDate <- ifelse(
  is.na(stores$Promo2SinceYear),NA,
  paste(stores$Promo2SinceYear,stores$Promo2SinceMonth,"01",sep = "-")
)

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

#combine the data
d <- merge(x=sales,y=stores, by= "Store",all.x = TRUE)

#split out some of the date parts for the sale date
d$Date = as.Date(d$Date,format="%Y-%m-%d")
d$SaleYear <- year(d$Date)
d$SaleMonth <- month(d$Date)
d$SaleWeek <- week(d$Date)

# determine if Promo2 was offered during the sale date:
d$IsPromo2Valid <- ifelse(
  d$Date >= d$Promo2StartDate
  & d$SaleMonth == d$P2M1 | d$SaleMonth == d$P2M2 | d$SaleMonth == d$P2M3 | d$SaleMonth == d$P2M4
  ,1,0
)

require(ggplot2)
ggplot(d,aes(factor(IsPromo2Valid),Sales,fill=IsPromo2Valid))+geom_boxplot()+ggtitle("Sales by Promo2")





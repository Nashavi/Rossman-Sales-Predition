require(lubridate)
dates<- seq(as.Date("2009/1/1"),as.Date("2016/1/1"),by="days")

calendar<-as.data.frame(dates)
calendar$dayofmonth<-as.numeric(substr(calendar$dates,9,10))
calendar$month<-month(calendar$dates)
calendar$quarter<-quarter(calendar$dates)
calendar$year<-year(calendar$dates)
calendar$week<-week(calendar$dates)
calendar$weekdays<-weekdays(calendar$dates)
calendar$dayofweek<-wday(calendar$dates)


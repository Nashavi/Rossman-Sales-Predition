#set working directory 

require(lubridate)

date <- as.factor(seq(as.Date("2008/01/01"),as.Date("2016/12/31"),"days"))

calendar <- as.data.frame(date)

calendar$date <- as.Date(calendar$date)
calendar$dayofYear <-as.numeric(strftime(calendar$date, format = "%j")) 
calendar$dayofWeek <- as.POSIXlt(calendar$date)$wday
calendar$weekday <- weekdays(calendar$date,abbreviate = "True")
calendar$dayofMonth <- day(calendar$date)
calendar$weekNum <- week(calendar$date)
calendar$month <- month(calendar$date)
calendar$quarter <- quarter(calendar$date)
calendar$year <- year(calendar$date)

date.lookup_by_year_weekday <- function(y,wn,wd) {
  datefinder <- calendar[calendar$year==y,] #subset calendar filtering on year
  datefinder <- datefinder[datefinder$weekNum==wn,] #subset on week number
  #datefinder <- datefinder[datefinder$month==m,]
  newdate <- datefinder[datefinder$weekday==wd,1] # match date against weekday (Mon,Tue,etc)
  return(as.Date(newdate)) #give back the date
}

stores <- read.csv(file="stores.csv")
attach(stores)

stores$CompetitionOpenDate <- as.Date(paste(CompetitionOpenSinceYear,CompetitionOpenSinceMonth,"01",sep = "-"))
stores$Promo2StartDate <- as.Date(Sys.Date())

for(i in 1:length(stores$Store)){
  y <- as.integer(ifelse(is.na(stores[i,"Promo2SinceYear"])=="TRUE",2016,stores[i,"Promo2SinceYear"]))
  wn <- as.integer(ifelse(is.na(stores[i,"Promo2SinceWeek"])=="TRUE",52,stores[i,"Promo2SinceWeek"]))
  p2d <- date.lookup_by_year_weekday(y,wn,"Mon")
  stores$Promo2StartDate[i] <- as.Date(p2d)
}
stores$Promo2StartDate <- as.Date(ifelse(is.na(stores$Promo2SinceYear)
                                         ,NA,stores$Promo2StartDate),origin="1970-01-01")
  

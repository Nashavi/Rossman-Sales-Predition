require(lubridate)

#first, create a calendar reference we can use to inpute missing dates 
Date <- as.factor(seq(as.Date("2008/01/01"),as.Date("2016/12/31"),"days"))
calendar <- as.data.frame(Date)
#calendar$Date <- as.Date(calendar$Date)
calendar$dayofYear <-as.numeric(strftime(calendar$Date, format = "%j")) 
calendar$dayofWeek <- as.POSIXlt(calendar$Date)$wday
calendar$weekday <- weekdays(as.Date(calendar$Date),abbreviate = "True")
calendar$dayofMonth <- day(calendar$Date)
calendar$weekNum <- week(calendar$Date)
calendar$month <- month(calendar$Date)
calendar$quarter <- quarter(calendar$Date)
calendar$year <- year(calendar$Date)

#create a function which takes a year, week number and weeday and returns a date 
date.lookup_by_year_weekday <- function(y,wn,wd) {
  datefinder <- calendar[calendar$year==y,] #subset calendar filtering on year
  datefinder <- datefinder[datefinder$weekNum==wn,] #subset on week number
  #datefinder <- datefinder[datefinder$month==m,]
  newdate <- datefinder[datefinder$weekday==wd,1] # match date against weekday (Mon,Tue,etc)
  return(as.Date(newdate)) #give back the date
}

#now derive some features on the stores dataframe 
stores <- read.csv(file="Datasets/CSV Files/stores.csv",na.strings = c("","NA"))

attach(stores)
stores <- stores[,-1]
#stores$Store <- as.factor(stores$Store)
#stores$Promo2 <- as.factor(stores$Promo2)
stores$State <- stores$StateCode


#assume the comp open date is the first of the month - a simple combo of year - month - 01
### THERE IS A STORE WITH A COMP OPEN DATE OF 1900! be careful!
unique(stores[stores$Store==551,"State"])
cosy <- as.data.frame(table(stores[is.na(stores$CompetitionOpenSinceYear)==FALSE & stores$State=="BY","CompetitionOpenSinceYear"]))
stores[stores$Store==551,"CompetitionOpenSinceYear"] <- 2012

stores$CompetitionOpenDate <- as.Date(paste(CompetitionOpenSinceYear,CompetitionOpenSinceMonth,"01",sep = "-"))

#figure out the promo2 start date: 
#first, set it to today's date 
stores$Promo2StartDate <- as.Date(Sys.Date())
#now, loop through all stores, figure out the promo2 year, promo2 week, then use the inpute function to return a date
for(i in 1:length(stores$Store)){
  y <- as.integer(ifelse(is.na(stores[i,"Promo2SinceYear"])=="TRUE",2016,stores[i,"Promo2SinceYear"]))
  wn <- as.integer(ifelse(is.na(stores[i,"Promo2SinceWeek"])=="TRUE",52,stores[i,"Promo2SinceWeek"]))
  p2d <- date.lookup_by_year_weekday(y,wn,"Mon")
  stores$Promo2StartDate[i] <- as.Date(p2d)
}

#if there was an NA for promo2, reset the promo2 startdate to NA 
stores$Promo2StartDate <- as.Date(ifelse(is.na(stores$Promo2SinceYear)
                                         ,NA,stores$Promo2StartDate),origin="1970-01-01")

### now lets add some additional features to the sales data 
#read in the sales data

train = read.csv("Datasets/CSV Files/train.csv",na.strings = c("","NA"))
test <- read.csv("Datasets/CSV Files/test.csv",na.strings = c("","NA"))
train$StateHoliday <- as.character(ifelse(train$StateHoliday==0,"0","1"))
train$StateHoliday <- as.factor(train$StateHoliday)
unique(train$StateHoliday)
test$Sales <- -1 #add sales to test 

train <- train[,-5] #remove customers 

train$Id <- seq.int(nrow(train)) #add ID column to train
names(train)
names(test)

sales <- rbind(train,test)
sales$testind <- ifelse(sales$Sales==-1,1,0)

length(sales[sales$testind==1,"Id"]) #check to make sure # rows is correct

attach(sales)
sales$Store <- as.factor(sales$Store)
#sales$Date  <- as.Date(sales$Date)
sales$Open <- as.factor(sales$Open)
sales$Promo <- as.factor(sales$Promo)
sales$StateHoliday <- as.factor(sales$StateHoliday)
sales$SchoolHoliday <- as.factor(sales$SchoolHoliday)

# since we already derived date parts in the calendar dataframe, lets grab that info via a merge: 
sales <- merge(sales,calendar,by="Date",all.x = TRUE)
#now just remove duplicated columns from merge
#sales = sales[,-which(names(sales) %in% c("date","DayOfWeek"))] 


#now merge sales and stores into a master dataframe
d <- merge(x=sales,y=stores, by= "Store",all.x = TRUE)

d[d$Id==907 & d$testind==1,]
d$Date <- as.Date(d$Date)
d <- d[order(d$Id),]
#Id <- d$Id
#d<-d[,-3]
#d<- cbind(Id,d)
#d[d$Id==907,]
d <- d[,-which(names(d) %in% c("DayOfWeek","weekday","dayofYear","StateCode","StateName","Customers"))]
d$dayofWeek <- as.factor(d$dayofWeek)
d$dayofMonth <- as.factor(d$dayofMonth)
d$weekNum <- as.factor(d$weekNum)
d$month <- as.factor(d$month)
d$quarter <- as.factor(d$quarter)
d$year <- as.factor(d$year)
d$CompetitionOpenSinceMonth <- as.factor(d$CompetitionOpenSinceMonth)
d$CompetitionOpenSinceYear <- as.factor(d$CompetitionOpenSinceYear)
d$Promo2 <- as.factor(d$Promo2)
d$Promo2SinceWeek <- as.factor(d$Promo2SinceWeek)
d$Promo2SinceYear <- as.factor(d$Promo2SinceYear)

Data <- d

save(Data,file="Datasets/CombinedData.RData",compress=TRUE)

d <- Data
rm(Data)

#######Feature Engineering
## Derive number of weeks between sale date and competition open date
d$WeeksSinceCompOpened <- as.integer(difftime(d$Date,d$CompetitionOpenDate,units="weeks"))
#if the comp wasn't open yet, set the value to 0 
d$WeeksSinceCompOpened <- ifelse(d$WeeksSinceCompOpened < 0, 0,d$WeeksSinceCompOpened)

#### Is Promo 2 Going On for a given Sales Date??
# first figure out if the sale date is after the promo2 Start Date
d$Promo2Valid <- as.factor(ifelse(difftime(d$Date,d$Promo2StartDate,units="days")>1,1,0))

#now, create a variable to match against promo2 interval using the sale month: 
# levels(d$PromoInterval) # as a reference 
d$SalesMonthInterval <- ifelse(
  d$month == 1 | d$month == 4 | d$month == 7 | d$month == 10, "Jan,Apr,Jul,Oct",
  ifelse(
    d$month == 2 | d$month == 5 | d$month == 8 | d$month == 11, "Feb,May,Aug,Nov",
  ifelse(
    d$month == 3 | d$month == 6 | d$month == 9 | d$month == 12, "Mar,Jun,Sept,Dec",
  NA)))

d$Promo2Valid <- ifelse(d$PromoInterval==d$SalesMonthInterval & d$Promo2Valid==1,1,0)

d$Promo2Valid <- as.factor(d$Promo2Valid)

d$WeeksSinceCompOpened <- as.integer(d$WeeksSinceCompOpened)

Data <- d
rm(d)
save(Data,file="Datasets/CombinedData.RData",compress=TRUE)







load("Datasets/Test.RData")

t<- TestData
rm(TestData)

t$Date <- as.Date(t$Date,'%Y-%m-%d')


t[t$Id==907,]

t$max_t <- -1
t$avg_t <- -1
t$min_t <- -1
t$max_wind <- -1
t$precip <- -1
t$cloudcover <- -1
t$events <- ""

airports <- unique(t[t$max_t==-1,"AirportCode"])

#we'll process one airport at a time:
for(a in 1:length(airports)){
  ac <- airports[a]
  
  #generate a vecotor of stores for that airport code:
  stores <- unique(t[t$AirportCode==ac,"Store"])
  
  #weather data package can only handle so many dates, so grab a vector of years to process:
  years <- unique(t[t$Store %in% stores,"year"])
  
  #print(paste("Processing",year,"year for",ac,"airport"))
  #now process one year at a time:
  
  for(k in 1:length(years)){
    year <- years[k] 
    
    #what is the start date we should use?
    begin <- min(t[t$Store %in% stores & t$year==year,"Date"])
    #what is the end date to use?
    end <- max(t[t$Store %in% stores & t$year==year,"Date"])
   
    #now grab weather for the airport using begin and end dates
    dat <- as.data.frame(weatherData::getWeatherForDate(ac,begin,end,opt_all_columns=TRUE))
    dat$Date <- as.Date(dat$Date)
    
    print(paste("Processing all dates for",ac," airport"))
    
    #loop through all weather dates and grab weather info for each one
    for(j in 1:length(dat$Date)){ #now for each date in the weather data
      dt <- dat$Date[j]
      dt <- as.Date(dt,'%Y-%m-%d')
      
      #print(paste("Processing Date: ",dt)) #optional if you want to see detailed processing
      #get weather features for the given date/airport: 
      max_t <- dat$Max_TemperatureF[j]
      avg_t <- dat$Mean_TemperatureF[j]
      min_t <- dat$Min_TemperatureF[j]  
      max_wind <- dat$Max_Wind_SpeedMPH[j] 
      precip <- dat$PrecipitationIn[j] 
      cloudcover <- dat$CloudCover[j] 
      events <- dat$Events[j]
      
      #now assign those weather features to stores:
      #match on storeid, airportcode, and date
      t[t$Store %in% stores & t$AirportCode==ac & t$Date==dt,"max_t"] <- max_t
      t[t$Store %in% stores & t$AirportCode==ac & t$Date==dt,"avg_t"] <- avg_t
      t[t$Store %in% stores & t$AirportCode==ac & t$Date==dt,"min_t"] <- min_t
      t[t$Store %in% stores & t$AirportCode==ac & t$Date==dt,"max_wind"] <- max_wind
      t[t$Store %in% stores & t$AirportCode==ac & t$Date==dt,"precip"] <- precip
      t[t$Store %in% stores & t$AirportCode==ac & t$Date==dt,"cloudcover"] <- cloudcover
      t[t$Store %in% stores & t$AirportCode==ac & t$Date==dt,"events"] <- events
    }
    print(paste("Done processing data for",ac,", year",year))
  }
}
print("Done")

t[t$Id==907,]

TestData <- t
rm(t)
save(TestData,file="Datasets/Test.RData",compress = TRUE)

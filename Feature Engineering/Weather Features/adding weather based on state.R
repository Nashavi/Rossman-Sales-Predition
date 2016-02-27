require(weatherData)
d <- read.csv("masterdataset.csv")
ss <- read.csv("StoreLocations.csv")

#merge the new store including airport codeswith the existing data:
d <- merge(d,ss,by="Store",all.x=TRUE)
#make sure the date variable is properly formatted:
d$Date <- as.Date(d$Date,'%Y-%m-%d')

# innitialize new variables in master data:
d$max_t <- -1
d$avg_t <- -1
d$min_t <- -1
d$max_wind <- -1
d$precip <- -1
d$cloudcover <- -1
d$events <- ""

#generate a vector of airport codes to process:  
airports <- unique(d[d$max_t==-1,"AirportCode"])

#we'll process one airport at a time:
for(a in 1:length(airports)){
   ac <- airports[a]
   
   #generate a vecotor of stores for that airport code:
   stores <- unique(d[d$AirportCode==ac,"Store"])
   
   #weather data package can only handle so many dates, so grab a vector of years to process:
   years <- unique(d[d$Store %in% stores,"year"])
   
   print(paste("Processing",year,"year for",ac,"airport"))
   #now process one year at a time:
   for(k in 1:length(years)){
     year <- years[k] 
     
     #what is the start date we should use?
     begin <- min(d[d$Store %in% stores & d$year==year,"Date"])
     #what is the end date to use?
     end <- max(d[d$Store %in% stores & d$year==year,"Date"])
     
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
        d[d$Store %in% stores & d$AirportCode==ac & d$Date==dt,"max_t"] <- max_t
        d[d$Store %in% stores & d$AirportCode==ac & d$Date==dt,"avg_t"] <- avg_t
        d[d$Store %in% stores & d$AirportCode==ac & d$Date==dt,"min_t"] <- min_t
        d[d$Store %in% stores & d$AirportCode==ac & d$Date==dt,"max_wind"] <- max_wind
        d[d$Store %in% stores & d$AirportCode==ac & d$Date==dt,"precip"] <- precip
        d[d$Store %in% stores & d$AirportCode==ac & d$Date==dt,"cloudcover"] <- cloudcover
        d[d$Store %in% stores & d$AirportCode==ac & d$Date==dt,"events"] <- events
      }
      print(paste("Done processing data for",ac,", year",year))
   }
  }
print("Done")

#now write the date to a new csv file. 
write.csv(d,file="MasterDataWithWeather.csv")

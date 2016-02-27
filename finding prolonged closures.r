require(lubridate)
d <- read.csv("masterdataset.csv")
s <- read.csv("stores.csv")
attach(d)
d<- d[d$Open=="0",] #grab only those values where a store was closed
d<- d[d$dayofWeek %in% c(1,2,3,4,5),] #eliminate saturdays and sundays
d<- d[d$StateHoliday=="0",] #grab only values where there wasn't a holiday

d$Date <- as.Date(d$Date) #format as a date
d<-d[order(d$Store,d$Date),] #order by store and then state

stores <- as.factor(s$Store)
rm(s)

d$pc <- 0

d$pc <- as.integer(d$pc)for(j in 1:length(stores)){ 
  sd <- d[d$Store==j,]              #create a sub dataframe for the jth store
  if (length(sd$X)!=0){             #check to make sure there is data in the sub dataframe
    sd <- sd[order(sd$Date),]       #make sure the data is sorted by date
    for(i in 2:length(sd$X)){ 
      x <- sd$X[i]                  #figure out the id for the row
      x2 <- sd$X[i-1]               #figure out the id for the previous row
      end <- d[d$X==x,"Date"]       #grab date value for the row
      start <- d[d$X==x2,"Date"]    #grab date for previous row
      dt <- ifelse(i==1,0,ifelse(as.integer(end - start)==1,1,0)) #were there 2 days in a row that were closed?
      if (is.na(x) == FALSE){
        d[d$X==x,"pc"] <- dt        #assign variable value
      }
    }
    rm(sd)                          #remove sub dataframe
  }
}


pclosures <- d[d$pc==1,] #which values got flagged?

pcstores <- unique(pclosures$Store)

pcstores #stores with prolonged closures
 
##### TO DO STILL: 
# 1) use this info to find the last date of a closure
# 2) flag the next day following a closure as a "grand opening"








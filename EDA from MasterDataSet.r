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

qplot(Customers,Sales,d,main="Sales by Total Customers") #heteroskedastic increasing to the right
#why would the variation increase as #cust increases?
qplot(Customers,Sales,d,col=Promo)


qplot(WeeksSinceCompOpened,Sales,d)
qplot(WeeksSinceCompOpened,Sales,d,col=StoreType)

#time series stuff
ggplot(d,aes(factor(year),Sales,fill=year))+bp+ggtitle("Sales by Year")
ggplot(d,aes(factor(quarter),Sales,fill=quarter))+bp+ggtitle("Sales by Quarter")
ggplot(d,aes(factor(month),Sales,fill=month))+bp+ggtitle("Sales by Month")
ggplot(d,aes(factor(weekNum),Sales,fill=weekNum))+bp+ggtitle("Sales by Week of Year")+noleg
ggplot(d,aes(factor(dayofMonth),Sales,fill=dayofMonth))+bp+ggtitle("Sales by Day of Month")+noleg
ggplot(d,aes(factor(weekday),Sales,fill=weekday))+bp+noleg
qplot(as.integer(dayofYear),Sales,d)

ggplot(d,aes(factor(Open),Sales,fill=Open))+bp+facet_wrap(~weekday)
#closed <- d[d$Open=="0",]
#closed[closed$Sales>0,] #absolutely no sales when closed
#rm(closed)
d[d$Open=="0" & d$Sales>0,] #any sales when closed? NO 
open_no_sales <- d[d$Open=="1" & d$Sales==0,] #any days when open but no sales?...YES
## these are days when either 0 or only a few customers come in 

o <- d[d$StoreType=="a" & d$Customers>6000,] #whats going on with that outlier?
#1/22/2013, store type a, open, promo = yes, no holidays, 
o <- d[d$StoreType=="a" & d$year==2013 & d$Open==1 & d$Promo==1 & d$month=="1",]
#what happened on this date that drove so many customers into that store? Is it worthwhile? 
qplot(o$Customers,o$Sales,col=o$Assortment)

### CHECK OUT AGGREGATE TRENDS HERE - sub in your group to first part, see the agg trend below
tsales <- as.data.frame(aggregate(d$Sales,by=list(d$month),FUN=mean))
ggplot(tsales,aes(tsales$Group.1,tsales$x))+geom_point()
#ggplot(yt.views, aes(Date, Views)) + geom_line() +
#  scale_x_date(format = "%b-%Y") + xlab("") + ylab("Daily Views")
### big drop in sales from July through Nov 
### end aggregate trends

rm(bad) #<- d[d$weekNum==53,]
d$weekNum <- ifelse(d$weekNum==53,52,d$weekNum)

## regular bivariate stuff
ggplot(d,aes(factor(Open),Sales,fill=Open))+bp+ggtitle("Sales by Open")
ggplot(d,aes(factor(Promo),Sales,fill=Promo))+bp+ggtitle("Sales by Promo")
ggplot(d,aes(factor(StateHoliday),Sales,fill=StateHoliday))+bp+ggtitle("Sales by State Holiday")
ggplot(d,aes(factor(SchoolHoliday),Sales,fill=SchoolHoliday))+bp+ggtitle("Sales by School Holiday")
ggplot(d,aes(factor(StoreType),Sales,fill=StoreType))+bp+ggtitle("Sales by Store Type")
ggplot(d,aes(factor(Assortment),Sales,fill=Assortment))+bp+ggtitle("Sales by Assortment")
ggplot(d,aes(factor(Promo2Valid),Sales,fill=Promo2Valid))+bp+ggtitle("Sales by Promo2")
ggplot(d,aes(factor(CompetitionOpenSinceMonth),Sales,fill=CompetitionOpenSinceMonth))+bp+ggtitle("Sales by Comp Open Since Month")
ggplot(d,aes(factor(CompetitionOpenSinceYear),Sales,fill=CompetitionOpenSinceYear))+bp+ggtitle("Sales by Comp Open Since Year")
### decent variability here - do we trust the store that opened in 1900???


#faceted by StoreType - store type B has very distinct sales pattern
qplot(Customers,Sales,d,col=StoreType,main="Sales vary by Store Type and Total Customers")
ggplot(d,aes(Customers,Sales,color=factor(StoreType)))+geom_point()+facet_wrap(~StoreType)+ggtitle("Sales by Customers - Broken Out by Store Type")
ggplot(d,aes(factor(weekday),Sales,fill=weekday))+bp+facet_wrap(~StoreType)
ggplot(d,aes(factor(Promo),Sales,fill=Promo))+bp+facet_wrap(~StoreType)
ggplot(d,aes(factor(StateHoliday),Sales,fill=StateHoliday))+bp+facet_wrap(~StoreType)
ggplot(d,aes(factor(SchoolHoliday),Sales,fill=SchoolHoliday))+bp+facet_wrap(~StoreType)
ggplot(d,aes(factor(Assortment),Sales,fill=Assortment))+bp+facet_wrap(~StoreType) # b-to-b relationship
ggplot(d,aes(factor(Promo2Valid),Sales,fill=Promo2Valid))+bp+facet_wrap(~StoreType)
ggplot(d,aes(factor(year),Sales,fill=year))+bp+facet_wrap(~StoreType)


#Faceted by weekday #### assortment b is only sold by store B!!!!
ggplot(d,aes(factor(Assortment),Sales,fill=Assortment))+bp+facet_wrap(~weekday)
ggplot(d,aes(factor(Promo),Sales,fill=Promo))+bp+facet_wrap(~weekday) #promo was never valid during weekends
ggplot(d,aes(factor(SchoolHoliday),Sales,fill=SchoolHoliday))+bp+facet_wrap(~weekday)

### some modeling based on EDA 
mod <- lm(Sales~Customers+StoreType+Open+month+dayofWeek+Promo,d)
summary(mod)
plot(mod)

## do stepwise selection
options(contrasts = rep ("contr.treatment", 2))
full.model = lm(Sales~Customers+StoreType+Open+month+dayofWeek+Promo,data=d)
null.model = lm(Sales~1,data=d)
step.model = step(null.model,scope=list(upper=full.model),data=train,direction="both")
step.aic = as.data.frame(step.model$anova,row.names = c(rep(1:nrow(step.model$anova)))) #dump results to a data frame
step.aic
step.aic$variables_added = rep(1:nrow(step.model$anova)) #add an column for each variable added
ggplot(step.aic,aes(variables_added,AIC))+geom_point()+geom_line(color="red")+ggtitle("Stepwise AIC by Total Variables Added")+xlim(0,13)

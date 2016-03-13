load("Modeling/GBM Models/BW_GBMModel.RData")
load("Datasets/FinalTestData.RData")
d <- FinalTestData
rm(FinalTestData)

final.preds <- predict(gbmFit,newdata = d)




d$Promo.2Valid_L20 <- ifelse(d$promovalid_L2.0==1,1,0)


d$SchoolHoliday._L5c <- ifelse(d$SchoolHoliday._L51==1,1,0)

d$StateHoliday._L5c <- ifelse(d$StateHoliday_L5.2==1,1,0)

d$StateHoliday._L4c <- ifelse(d$StateHoliday_L4.1==1,1,0)

d$StateHoliday._L3c <- ifelse(d$StateHoliday_L3.1==1,1,0)
d$StateHoliday._L2c <- ifelse(d$StateHoliday_L2.1==1,1,0)
d$StateHoliday._L1c <- ifelse(d$StateHoliday_L1.1==1,1,0)

d$promovalid._L22 <- ifelse(d$promovalid_L2.2==1,1,0)

d$promovalid._L21 <- ifelse(d$promovalid_L2.1==1,1,0)
d$promovalid._L20 <- ifelse(d$promovalid_L2.0==1,1,0)


d$promovalid._L12 <- ifelse(d$promovalid_L2.1==1,1,0)

d$promovalid._L10 <- ifelse(d$promovalid_L1.1==1,0,1)
d$promovalid._L11 <- ifelse(d$promovalid_L1.1==1,1,0)

d$Open_L1.0 <- ifelse(d$Open_L1.1==1,0,1)
d$Open_L1.1 <- ifelse(d$Open_L1.1==1,1,0)


d$promovalid.0 <- ifelse(d$promovalid==1,0,1)
d$promovalid.1 <- ifelse(d$promovalid==1,1,0)

d$events.1 <- ifelse(d$events.==1,1,0)
d$events.2 <- ifelse(d$events.Fog==1,0,1)
d$events.3 <- ifelse(d$events.Rain==1,1,0)
d$events.4 <- ifelse(d$events.Fog.Rain==1,1,0)
d$events.5 <- ifelse(d$events.Thunderstorm==1,1,0)
d$events.6 <- ifelse(d$events.Rain.Thunderstorm==1,1,0)
d$events.7 <- ifelse(d$events.Fog.Rain.Thunderstorm==1,1,0)
d$events.8 <- ifelse(d$events.Fog.Rain.Thunderstorm==1,0,1)
d$events.9 <- ifelse(d$events.==1,0,1)
d$events.10 <- ifelse(d$events.1==1,0,1)
d$events.11 <- ifelse(d$events.2==1,0,1)
d$events.12 <-ifelse(d$events.2==1,0,1)
d$events.13 <- ifelse(d$events.3==1,0,1)
d$events.14 <- ifelse(d$events.4==1,0,1)
d$events.15 <- ifelse(d$events.5==1,0,1)
d$events.16 <- ifelse(d$events.6==1,0,1)
d$events.17 <- ifelse(d$events.7==1,0,1) 
d$events.18 <- ifelse(d$events.8==1,0,1)
d$events.19 <- ifelse(d$events.9==1,0,1)
d$events.20 <- ifelse(d$events.10==1,0,1)
d$events.21 <- ifelse(d$events.11==1,0,1)
d$events.22 <- ifelse(d$events.12==1,0,1)
d$events.22 <- ifelse(d$events.12==1,0,1)



d$weekNum.1 <- 0
d$weekNum.2 <- 0
d$weekNum.3 <- 0
d$weekNum.4 <- 0
d$weekNum.5 <- 0
d$weekNum.6 <- 0
d$weekNum.7 <- 0
d$weekNum.8 <- 0
d$weekNum.9 <- 0
d$weekNum.10 <- 0
d$weekNum.11 <- 0
d$weekNum.12 <- 0
d$weekNum.13 <- 0
d$weekNum.14 <- 0
d$weekNum.15 <- 0
d$weekNum.16 <- 0
d$weekNum.17 <- 0
d$weekNum.18 <- 0
d$weekNum.19 <- 0
d$weekNum.20 <- 0
d$weekNum.21 <- 0
d$weekNum.22 <- 0
d$weekNum.23 <- 0
d$weekNum.24 <- 0
# d$weekNum.25 <- 0
# d$weekNum.26 <- 0
# d$weekNum.27 <- 0
# d$weekNum.28 <- 0
# d$weekNum.29 <- 0
# d$weekNum.30 <- 0
# d$weekNum.31 <- 0
d$weekNum.32 <- 0
d$weekNum.33 <- 0
d$weekNum.34 <- 0
d$weekNum.35 <- 0
d$weekNum.36 <- 0
d$weekNum.37 <- 0
d$weekNum.38 <- 0
d$weekNum.39 <- 0
d$weekNum.40 <- 0
d$weekNum.41 <- 0
d$weekNum.42 <- 0
d$weekNum.43 <- 0
d$weekNum.44 <- 0
d$weekNum.45 <- 0
d$weekNum.46 <- 0
d$weekNum.47 <- 0
d$weekNum.48 <- 0
d$weekNum.49 <- 0
d$weekNum.50 <- 0
d$weekNum.51 <- 0
d$weekNum.52 <- 0
d$weekNum.53 <- 0

d$month.1 <- 0
d$month.2 <- 0
d$month.3 <- 0
d$month.4 <- 0
d$month.5 <- 0
d$month.8 <- 0
d$month.9 <- 0
d$month.10 <- 0
d$month.11 <- 0
d$month.12 <- 0

d$quarter.1 <- 0
d$quarter.4 <- 0










d$year.2013 <- 0
d$year.2014 <- 0
d$year.2015 <- 1





d <- FinalTestData
rm(FinalTestData)

d$StateHoliday.0 <- 1
d$StateHoliday.1 <- 0



d$weekday.Fri <- ifelse(d$dayofWeek.5==1,1,0)
d$weekday.Mon <- ifelse(d$dayofWeek.1==1,1,0)
d$weekday.Tue <- ifelse(d$dayofWeek.2==1,1,0)
d$weekday.Wed <- ifelse(d$dayofWeek.3==1,1,0)
d$weekday.Thu <- ifelse(d$dayofWeek.4==1,1,0)
d$weekday.Sat <- ifelse(d$dayofWeek.6==1,1,0)
d$weekday.Sun <- ifelse(d$dayofWeek.0==1,1,0)








d$Promo.2Valid2 <- 0





d$p


d$Open_L1.1 <- ifelse(d$Open._L11==1,1,0)
d$Open_L1.0 <-  ifelse(d$Open._L11==1,0,1)
d$Open_L1.2 <- ifelse(d$Open._L12==1,1,0)


d$StateHoliday._L10 <- 0
d$StateHoliday._L12 <- 0
d$StateHoliday._L1a <- 0
d$StateHoliday._L1b <- 0
d$StateHoliday._L1c <- 0
d$StateHoliday._L20 <- 0
d$StateHoliday._L22 <- 0
d$StateHoliday._L2a <- 0
d$StateHoliday._L2b <- 0
d$StateHoliday._L2c <- 0
d$StateHoliday._L30 <- 0
d$StateHoliday._L32 <- 0
d$StateHoliday._L3a <- 0
d$StateHoliday._L3b <- 0
d$StateHoliday._L3c <- 0
d$StateHoliday._L40 <- 0
d$StateHoliday._L4a <- 0
d$StateHoliday._L4b <- 0
d$StateHoliday._L4c <- 0
d$StateHoliday._L50 <- 0
d$StateHoliday._L5a <- 0
d$StateHoliday._L5b <- 0
d$StateHoliday._L5c <- 0
d$StateHoliday._L52 <- 0


d$SchoolHoliday._L1b <- 0
d$SchoolHoliday._L1c <- 0

d$SchoolHoliday._L2b <- 0
d$SchoolHoliday._L2c <- 0

d$SchoolHoliday._L3b <- 0
d$SchoolHoliday._L3c <- 0

d$SchoolHoliday._L4b <- 0
d$SchoolHoliday._L4c <- 0


d$SchoolHoliday._L5b <- 0
d$SchoolHoliday._L5c <- 0

Id <- d$Id
Store <- d$Store 
Open <- d$Open
Sales <- ifelse(d$Open==1,exp(final.preds),0)

preds <- cbind(Open,Sales)
preds <- cbind(Store,preds)
preds <- cbind(Id,preds)

write.csv(preds,file="Prediction Files/Week 2 New Preds.csv")


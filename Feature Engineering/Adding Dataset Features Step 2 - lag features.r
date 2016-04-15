require(DataCombine)

load("Datasets/CombinedData.RData")
d<- Data
rm(Data)

d<-d[order(d$Store,d$Date),] #order by store and then dtate
d$Date <- as.Date(d$Date)

d$promovalid <- ifelse(d$Promo=="1" & d$Open=="1",1,0)
d$promovalid <- factor(d$promovalid,levels = c("0","1"),labels = c("0","1"))

d$Promo2Valid <- ifelse(is.na(d$Promo2Valid)==TRUE,"2",d$Promo2Valid)
d$Promo2Valid <- factor(d$Promo2Valid,levels=c("0","1","2"))


#Open lag by 1, 2, 3
dlagged <- slide(d, Var = "Open", GroupVar = "Store", NewVar= "OpenLag1", slideBy = -1)
dlagged <- slide(dlagged, Var = "Open", GroupVar = "Store", NewVar= "OpenLag2", slideBy = -2)
dlagged <- slide(dlagged, Var = "Open", GroupVar = "Store", NewVar= "OpenLag3", slideBy = -3)

#Open lead by 1, 2, 3 
dlagged <- slide(dlagged, Var = "Open", GroupVar = "Store", NewVar= "OpenLead1", slideBy = 1)
dlagged <- slide(dlagged, Var = "Open", GroupVar = "Store", NewVar= "OpenLead2", slideBy = 2)
dlagged <- slide(dlagged, Var = "Open", GroupVar = "Store", NewVar= "OpenLead3", slideBy = 3)

#Promovalid lag by 1, 2, 3 
dlagged<- slide(dlagged, Var = "promovalid", GroupVar = "Store", NewVar= "PromoValidLag1", slideBy = -1)
dlagged<- slide(dlagged, Var = "promovalid", GroupVar = "Store", NewVar= "PromoValidLag2",slideBy = -2)
dlagged<- slide(dlagged, Var = "promovalid", GroupVar = "Store", NewVar= "PromoValidLag3",slideBy = -3)

#Promovalid lead by 1, 2, 3 
dlagged<- slide(dlagged, Var = "promovalid", GroupVar = "Store", NewVar= "PromoValidLead1", slideBy = 1)
dlagged<- slide(dlagged, Var = "promovalid", GroupVar = "Store", NewVar= "PromoValidLead2",slideBy = 2)
dlagged<- slide(dlagged, Var = "promovalid", GroupVar = "Store", NewVar= "PromoValidLead3",slideBy = 3)

#Promo2valid lag by 1, 2, 3 
dlagged<- slide(dlagged, Var = "Promo2Valid", GroupVar = "Store", NewVar= "Promo2ValidLag1", slideBy = -1)
dlagged<- slide(dlagged, Var = "Promo2Valid", GroupVar = "Store", NewVar= "Promo2ValidLag2",slideBy = -2)
dlagged<- slide(dlagged, Var = "Promo2Valid", GroupVar = "Store", NewVar= "Promo2ValidLag3",slideBy = -3)

#Promo2valid lead by 1, 2, 3 
dlagged<- slide(dlagged, Var = "Promo2Valid", GroupVar = "Store", NewVar= "Promo2ValidLead1", slideBy = 1)
dlagged<- slide(dlagged, Var = "Promo2Valid", GroupVar = "Store", NewVar= "Promo2ValidLead2",slideBy = 2)
dlagged<- slide(dlagged, Var = "Promo2Valid", GroupVar = "Store", NewVar= "Promo2ValidLead3",slideBy = 3)

#StateHolidays lag by 1 to 5
dlagged<- slide(dlagged, Var = "StateHoliday", GroupVar = "Store", NewVar= "StateHolidayLag1", slideBy = -1)
dlagged<- slide(dlagged, Var = "StateHoliday", GroupVar = "Store", NewVar= "StateHolidayLag2", slideBy = -2)
dlagged<- slide(dlagged, Var = "StateHoliday", GroupVar = "Store", NewVar= "StateHolidayLag3", slideBy = -3)
dlagged<- slide(dlagged, Var = "StateHoliday", GroupVar = "Store", NewVar= "StateHolidayLag4", slideBy = -4)
dlagged<- slide(dlagged, Var = "StateHoliday", GroupVar = "Store", NewVar= "StateHolidayLag5", slideBy = -5)

#StateHolidays lead by 1 to 5
dlagged<- slide(dlagged, Var = "StateHoliday", GroupVar = "Store", NewVar= "StateHolidayLead1", slideBy = 1)
dlagged<- slide(dlagged, Var = "StateHoliday", GroupVar = "Store", NewVar= "StateHolidayLead2", slideBy = 2)
dlagged<- slide(dlagged, Var = "StateHoliday", GroupVar = "Store", NewVar= "StateHolidayLead3", slideBy = 3)
dlagged<- slide(dlagged, Var = "StateHoliday", GroupVar = "Store", NewVar= "StateHolidayLead4", slideBy = 4)
dlagged<- slide(dlagged, Var = "StateHoliday", GroupVar = "Store", NewVar= "StateHolidayLead5", slideBy = 5)

#SchoolHolidays lag by 1 to 5
dlagged<- slide(dlagged, Var = "SchoolHoliday", GroupVar = "Store", NewVar= "SchoolHolidayLag1", slideBy = -1)
dlagged<- slide(dlagged, Var = "SchoolHoliday", GroupVar = "Store", NewVar= "SchoolHolidayLag2", slideBy = -2)
dlagged<- slide(dlagged, Var = "SchoolHoliday", GroupVar = "Store", NewVar= "SchoolHolidayLag3", slideBy = -3)
dlagged<- slide(dlagged, Var = "SchoolHoliday", GroupVar = "Store", NewVar= "SchoolHolidayLag4", slideBy = -4)
dlagged<- slide(dlagged, Var = "SchoolHoliday", GroupVar = "Store", NewVar= "SchoolHolidayLag5", slideBy = -5)

#SchoolHolidays lead by 1 to 5
dlagged<- slide(dlagged, Var = "SchoolHoliday", GroupVar = "Store", NewVar= "SchoolHolidayLead1", slideBy = 1)
dlagged<- slide(dlagged, Var = "SchoolHoliday", GroupVar = "Store", NewVar= "SchoolHolidayLead2", slideBy = 2)
dlagged<- slide(dlagged, Var = "SchoolHoliday", GroupVar = "Store", NewVar= "SchoolHolidayLead3", slideBy = 3)
dlagged<- slide(dlagged, Var = "SchoolHoliday", GroupVar = "Store", NewVar= "SchoolHolidayLead4", slideBy = 4)
dlagged<- slide(dlagged, Var = "SchoolHoliday", GroupVar = "Store", NewVar= "SchoolHolidayLead5", slideBy = 5)

#Sales LAGS 
lag = 6*7
dlagged<- slide(dlagged, Var = "Sales", GroupVar = "Store", NewVar= "SixWeekSalesLag",slideBy = -lag)
lag = 7*7
dlagged<- slide(dlagged, Var = "Sales", GroupVar = "Store", NewVar= "SevenWeekSalesLag",slideBy = -lag)
lag = 8*7
dlagged<- slide(dlagged, Var = "Sales", GroupVar = "Store", NewVar= "EightWeekSalesLag",slideBy = -lag)
lag = 9*7
dlagged<- slide(dlagged, Var = "Sales", GroupVar = "Store", NewVar= "NineWeekSalesLag",slideBy = -lag)
lag = 10*7
dlagged<- slide(dlagged, Var = "Sales", GroupVar = "Store", NewVar= "TenWeekSalesLag",slideBy = -lag)


Data <- dlagged


save(Data,file="Datasets/CombinedData.RData",compress = TRUE)

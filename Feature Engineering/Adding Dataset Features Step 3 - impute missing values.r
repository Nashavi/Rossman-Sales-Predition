load("Datasets/CombinedData.RData")
d<- Data
rm(Data)

m = matrix(data=NA,nrow=length(names(d)),ncol=2)
dimnames(m) = list(names(d),c("Total Missing","Percent Missing"))        
for(i in 1:nrow(m)){
  l = length(d[,i])
  u = length(which(is.na(d[,i])))
  m[i,"Total Missing"] = u
  m[i,"Percent Missing"] = round(u/l,digits=4)
}
m <- data.frame(m[m[,"Percent Missing"]!=0,])


#Open Lead and Lag
d$OpenLag1 <- ifelse(is.na(d$OpenLag1)==TRUE,3,d$OpenLag1)
d$OpenLag1 <- factor(d$OpenLag1,levels=c("1","2","3"))
d$OpenLag2 <- ifelse(is.na(d$OpenLag2)==TRUE,3,d$OpenLag2)
d$OpenLag2 <- factor(d$OpenLag2,levels=c("1","2","3"))
d$OpenLag3 <- ifelse(is.na(d$OpenLag3)==TRUE,3,d$OpenLag3)
d$OpenLag3 <- factor(d$OpenLag3,levels=c("1","2","3"))

d$OpenLead1 <- ifelse(is.na(d$OpenLead1)==TRUE,3,d$OpenLead1)
d$OpenLead1 <- factor(d$OpenLead1,levels=c("1","2","3"))
d$OpenLead2 <- ifelse(is.na(d$OpenLead2)==TRUE,3,d$OpenLead2)
d$OpenLead2 <- factor(d$OpenLead2,levels=c("1","2","3"))
d$OpenLead3 <- ifelse(is.na(d$OpenLead3)==TRUE,3,d$OpenLead3)
d$OpenLead3 <- factor(d$OpenLead3,levels=c("1","2","3"))

#Promo Valid Lead and Lag 
d$PromoValidLag1 <- ifelse(is.na(d$PromoValidLag1)==TRUE,3,d$PromoValidLag1)
d$PromoValidLag1 <- factor(d$PromoValidLag1,levels=c("1","2","3"))
d$PromoValidLag2 <- ifelse(is.na(d$PromoValidLag2)==TRUE,3,d$PromoValidLag2)
d$PromoValidLag2 <- factor(d$PromoValidLag2,levels=c("1","2","3"))
d$PromoValidLag3 <- ifelse(is.na(d$PromoValidLag3)==TRUE,3,d$PromoValidLag3)
d$PromoValidLag3 <- factor(d$PromoValidLag3,levels=c("1","2","3"))

d$PromoValidLead1 <- ifelse(is.na(d$PromoValidLead1)==TRUE,3,d$PromoValidLead1)
d$PromoValidLead1 <- factor(d$PromoValidLead1,levels=c("1","2","3"))
d$PromoValidLead2 <- ifelse(is.na(d$PromoValidLead2)==TRUE,3,d$PromoValidLead2)
d$PromoValidLead2 <- factor(d$PromoValidLead2,levels=c("1","2","3"))
d$PromoValidLead3 <- ifelse(is.na(d$PromoValidLead3)==TRUE,3,d$PromoValidLead3)
d$PromoValidLead3 <- factor(d$PromoValidLead3,levels=c("1","2","3"))


#State Holiday 
d$StateHolidayLag1 <- ifelse(is.na(d$StateHolidayLag1)==TRUE,"3",d$StateHolidayLag1)
d$StateHolidayLag1 <- factor(d$StateHolidayLag1,levels=c("1","2","3"))
d$StateHolidayLag2 <- ifelse(is.na(d$StateHolidayLag2)==TRUE,"3",d$StateHolidayLag2)
d$StateHolidayLag2 <- factor(d$StateHolidayLag2,levels=c("1","2","3"))
d$StateHolidayLag3 <- ifelse(is.na(d$StateHolidayLag3)==TRUE,"3",d$StateHolidayLag3)
d$StateHolidayLag3 <- factor(d$StateHolidayLag3,levels=c("1","2","3")) 
d$StateHolidayLag4 <- ifelse(is.na(d$StateHolidayLag4)==TRUE,"3",d$StateHolidayLag4)
d$StateHolidayLag4 <- factor(d$StateHolidayLag4,levels=c("1","2","3"))
d$StateHolidayLag5 <- ifelse(is.na(d$StateHolidayLag5)==TRUE,"3",d$StateHolidayLag5)
d$StateHolidayLag5 <- factor(d$StateHolidayLag5,levels=c("1","2","3"))

d$StateHolidayLead1 <- ifelse(is.na(d$StateHolidayLead1)==TRUE,"3",d$StateHolidayLead1)
d$StateHolidayLead1 <- factor(d$StateHolidayLead1,levels=c("1","2","3"))
d$StateHolidayLead2 <- ifelse(is.na(d$StateHolidayLead2)==TRUE,"3",d$StateHolidayLead2)
d$StateHolidayLead2 <- factor(d$StateHolidayLead2,levels=c("1","2","3"))
d$StateHolidayLead3 <- ifelse(is.na(d$StateHolidayLead3)==TRUE,"3",d$StateHolidayLead3)
d$StateHolidayLead3 <- factor(d$StateHolidayLead3,levels=c("1","2","3")) 
d$StateHolidayLead4 <- ifelse(is.na(d$StateHolidayLead4)==TRUE,"3",d$StateHolidayLead4)
d$StateHolidayLead4 <- factor(d$StateHolidayLead4,levels=c("1","2","3"))
d$StateHolidayLead5 <- ifelse(is.na(d$StateHolidayLead5)==TRUE,"3",d$StateHolidayLead5)
d$StateHolidayLead5 <- factor(d$StateHolidayLead5,levels=c("1","2","3"))


#School Holiday 
d$SchoolHolidayLag1 <- ifelse(is.na(d$SchoolHolidayLag1)==TRUE,"3",d$SchoolHolidayLag1)
d$SchoolHolidayLag1 <- factor(d$SchoolHolidayLag1,levels=c("1","2","3"))
d$SchoolHolidayLag2 <- ifelse(is.na(d$SchoolHolidayLag2)==TRUE,"3",d$SchoolHolidayLag2)
d$SchoolHolidayLag2 <- factor(d$SchoolHolidayLag2,levels=c("1","2","3"))
d$SchoolHolidayLag3 <- ifelse(is.na(d$SchoolHolidayLag3)==TRUE,"3",d$SchoolHolidayLag3)
d$SchoolHolidayLag3 <- factor(d$SchoolHolidayLag3,levels=c("1","2","3")) 
d$SchoolHolidayLag4 <- ifelse(is.na(d$SchoolHolidayLag4)==TRUE,"3",d$SchoolHolidayLag4)
d$SchoolHolidayLag4 <- factor(d$SchoolHolidayLag4,levels=c("1","2","3"))
d$SchoolHolidayLag5 <- ifelse(is.na(d$SchoolHolidayLag5)==TRUE,"3",d$SchoolHolidayLag5)
d$SchoolHolidayLag5 <- factor(d$SchoolHolidayLag5,levels=c("1","2","3"))

d$SchoolHolidayLead1 <- ifelse(is.na(d$SchoolHolidayLead1)==TRUE,"3",d$SchoolHolidayLead1)
d$SchoolHolidayLead1 <- factor(d$SchoolHolidayLead1,levels=c("1","2","3"))
d$SchoolHolidayLead2 <- ifelse(is.na(d$SchoolHolidayLead2)==TRUE,"3",d$SchoolHolidayLead2)
d$SchoolHolidayLead2 <- factor(d$SchoolHolidayLead2,levels=c("1","2","3"))
d$SchoolHolidayLead3 <- ifelse(is.na(d$SchoolHolidayLead3)==TRUE,"3",d$SchoolHolidayLead3)
d$SchoolHolidayLead3 <- factor(d$SchoolHolidayLead3,levels=c("1","2","3")) 
d$SchoolHolidayLead4 <- ifelse(is.na(d$SchoolHolidayLead4)==TRUE,"3",d$SchoolHolidayLead4)
d$SchoolHolidayLead4 <- factor(d$SchoolHolidayLead4,levels=c("1","2","3"))
d$SchoolHolidayLead5 <- ifelse(is.na(d$SchoolHolidayLead5)==TRUE,"3",d$SchoolHolidayLead5)
d$SchoolHolidayLead5 <- factor(d$SchoolHolidayLead5,levels=c("1","2","3"))


#Promo2Valid
d$Promo2ValidLag1 <- as.character(d$Promo2ValidLag1)
d$Promo2ValidLag1 <- ifelse(is.na(d$Promo2ValidLag1)==TRUE,"3",d$Promo2ValidLag1)
d$Promo2ValidLag1 <- factor(d$Promo2ValidLag1,levels=c("1","2","3"))
d$Promo2ValidLag2 <- as.character(d$Promo2ValidLag2)
d$Promo2ValidLag2 <- ifelse(is.na(d$Promo2ValidLag2)==TRUE,"3",d$Promo2ValidLag2)
d$Promo2ValidLag2 <- factor(d$Promo2ValidLag2,levels=c("1","2","3"))
d$Promo2ValidLag3 <- as.character(d$Promo2ValidLag3)
d$Promo2ValidLag3 <- ifelse(is.na(d$Promo2ValidLag3)==TRUE,"3",d$Promo2ValidLag3)
d$Promo2ValidLag3 <- factor(d$Promo2ValidLag3,levels=c("1","2","3"))

d$Promo2ValidLead1 <- as.character(d$Promo2ValidLead1)
d$Promo2ValidLead1 <- ifelse(is.na(d$Promo2ValidLead1)==TRUE,"3",d$Promo2ValidLead1)
d$Promo2ValidLead1 <- factor(d$Promo2ValidLead1,levels=c("1","2","3"))
d$Promo2ValidLead2 <- as.character(d$Promo2ValidLead2)
d$Promo2ValidLead2 <- ifelse(is.na(d$Promo2ValidLead2)==TRUE,"3",d$Promo2ValidLead2)
d$Promo2ValidLead2 <- factor(d$Promo2ValidLead2,levels=c("1","2","3"))
d$Promo2ValidLead3 <- as.character(d$Promo2ValidLead3)
d$Promo2ValidLead3 <- ifelse(is.na(d$Promo2ValidLead3)==TRUE,"3",d$Promo2ValidLead3)
d$Promo2ValidLead3 <- factor(d$Promo2ValidLead3,levels=c("1","2","3"))

#Competition Distance
stores <- unique(d[is.na(d$CompetitionDistance),"Store"])
for(i in stores) {
  st <- unique(d[d$Store==i,"State"])
  avgdist <- mean(d[d$State==st & is.na(d$CompetitionDistance)==FALSE,"CompetitionDistance"])
  d[d$Store==i,"CompetitionDistance"] <- avgdist
}

d$CompetitionOpenSinceYear <- as.integer(as.character(d$CompetitionOpenSinceYear))
d$CompetitionOpenSinceMonth <- as.integer(as.character(d$CompetitionOpenSinceMonth))

stores <- unique(d[is.na(d$WeeksSinceCompOpened),"Store"])
for(i in stores) {
  st <- unique(d[d$Store==i,"State"])
  avg.yr <- round(mean(d[d$State==st & is.na(d$WeeksSinceCompOpened)==FALSE,"CompetitionOpenSinceYear"]),0)
  avg.mo <- round(mean(d[d$State==st & is.na(d$WeeksSinceCompOpened)==FALSE,"CompetitionOpenSinceMonth"]),0)
  #print(paste(i,st,avg.yr,avg.mo))
   d[d$Store==i,"CompetitionOpenSinceYear"] <- avg.yr
   d[d$Store==i,"CompetitionOpenSinceMonth"] <- avg.mo
}
d$CompetitionOpenDate <- as.Date(paste(d$CompetitionOpenSinceYear,d$CompetitionOpenSinceMonth,"01",sep = "-"))
d$WeeksSinceCompOpened <- as.integer(difftime(d$Date,d$CompetitionOpenDate,units="weeks"))

m[1]
cols.to.remove <- c("Promo2SinceWeek","Promo2SinceYear","PromoInterval","Promo2StartDate")

d <- d[,-which(names(d) %in% cols.to.remove)]

y <- unique(d[is.na(d$SixWeekSalesLag)==TRUE,"year"])
mo <- unique(d[is.na(d$SixWeekSalesLag)==TRUE,"month"])
ms <- mean(d[d$year==y & d$month %in% mo,"Sales"])
d$SixWeekSalesLag <- ifelse(is.na(d$SixWeekSalesLag)==TRUE,ms,d$SixWeekSalesLag)

y <- unique(d[is.na(d$SevenWeekSalesLag)==TRUE,"year"])
mo <- unique(d[is.na(d$SevenWeekSalesLag)==TRUE,"month"])
ms <- mean(d[d$year==y & d$month %in% mo,"Sales"])
d$SevenWeekSalesLag <- ifelse(is.na(d$SevenWeekSalesLag)==TRUE,ms,d$SevenWeekSalesLag)

y <- unique(d[is.na(d$EightWeekSalesLag)==TRUE,"year"])
mo <- unique(d[is.na(d$EightWeekSalesLag)==TRUE,"month"])
ms <- mean(d[d$year==y & d$month %in% mo,"Sales"])
d$EightWeekSalesLag <- ifelse(is.na(d$EightWeekSalesLag)==TRUE,ms,d$EightWeekSalesLag)

y <- unique(d[is.na(d$NineWeekSalesLag)==TRUE,"year"])
mo <- unique(d[is.na(d$NineWeekSalesLag)==TRUE,"month"])
ms <- mean(d[d$year==y & d$month %in% mo,"Sales"])
d$NineWeekSalesLag <- ifelse(is.na(d$NineWeekSalesLag)==TRUE,ms,d$NineWeekSalesLag)

y <- unique(d[is.na(d$TenWeekSalesLag)==TRUE,"year"])
mo <- unique(d[is.na(d$TenWeekSalesLag)==TRUE,"month"])
ms <- mean(d[d$year==y & d$month %in% mo,"Sales"])
d$TenWeekSalesLag <- ifelse(is.na(d$TenWeekSalesLag)==TRUE,ms,d$TenWeekSalesLag)

m = matrix(data=NA,nrow=length(names(d)),ncol=2)
dimnames(m) = list(names(d),c("Total Missing","Percent Missing"))        
for(i in 1:nrow(m)){
  l = length(d[,i])
  u = length(which(is.na(d[,i])))
  m[i,"Total Missing"] = u
  m[i,"Percent Missing"] = round(u/l,digits=4)
}
m <- data.frame(m[m[,"Percent Missing"]!=0,])

str(d)

Data <- d

save(Data,file="Datasets/CombinedData.RData",compress = TRUE)





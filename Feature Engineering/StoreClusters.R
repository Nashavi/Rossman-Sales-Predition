load("Datasets/TrainData.RData")
library(dplyr)
head(TrainData)

TrainData$SalesGroup <- -1

#Cluster on SD of store sales
var_sales<-as.data.frame(TrainData %>% group_by(Store) %>% summarise(varsales = var(Sales)) %>% arrange(varsales))
# vars_grp1<-var_sales[1:200,1]
# vars_grp2<-var_sales[201:400,1]
# vars_grp3<-var_sales[401:600,1]
# vars_grp4<-var_sales[601:800,1]
# vars_grp5<-var_sales[801:1000,1]
# 88049498 - 1689727
# 86359771/4
# 21589943 * 2
summary(var_sales)
grp.six <- unique(var_sales[var_sales$varsales>43179886,"Store"])

TrainData[TrainData$Store %in% grp.six,"SalesGroup"] <- 6
grp.five <- unique(var_sales[var_sales$varsales>21589943 & var_sales$varsales <43179886,"Store"])
TrainData[TrainData$Store %in% grp.five,"SalesGroup"] <- 5

#Cluster on avg daily sales
 avg_daily_sales<-as.data.frame(TrainData %>% group_by(Store) %>% summarise(avgdailysales = mean(Sales)) %>% arrange(avgdailysales))
avg.stores <- unique(TrainData[TrainData$SalesGroup==-1,"Store"])
 length(avg.stores)
 avg_daily_sales <- avg_daily_sales[avg_daily_sales$Store %in% avg.stores,]
 # avgs_grp1<-avg_daily_sales[1:200,1]
# avgs_grp2<-avg_daily_sales[201:400,1]
# avgs_grp3<-avg_daily_sales[401:600,1]
# avgs_grp4<-avg_daily_sales[601:800,1]
# avgs_grp5<-avg_daily_sales[801:1000,1]
summary(avg_daily_sales)

18307-2282
16025/4
4006.25*3
grp.four <- unique(avg_daily_sales[avg_daily_sales$avgdailysales>6384,"Store"])
TrainData[TrainData$Store %in% grp.four,"SalesGroup"] <- 4

grp.three <- unique(avg_daily_sales[avg_daily_sales$avgdailysales<6384 & avg_daily_sales$avgdailysales >5378,"Store"])
TrainData[TrainData$Store %in% grp.three,"SalesGroup"] <- 3

grp.two <- unique(avg_daily_sales[avg_daily_sales$avgdailysales<5378 & avg_daily_sales$avgdailysales >4404,"Store"])
TrainData[TrainData$Store %in% grp.two,"SalesGroup"] <- 2

grp.one <- unique(avg_daily_sales[avg_daily_sales$avgdailysales<4404 ,"Store"])
TrainData[TrainData$Store %in% grp.one,"SalesGroup"] <- 1

TrainData[TrainData$SalesGroup==-1,]

d <- as.data.frame(TrainData %>% group_by(Store) %>% summarise(SalesGroup = max(SalesGroup)))

save(d,file="Datasets/StoreSalesGroups.RData",compress = TRUE)


# 
# #Cluster on Max of store sales
# max_sales<-as.data.frame(GrandMasterData %>% group_by(Store) %>% summarise(maxsales = max(Sales)) %>% arrange(maxsales))
# maxs_grp1<-max_sales[1:200,1]
# maxs_grp2<-max_sales[201:400,1]
# maxs_grp3<-max_sales[401:600,1]
# maxs_grp4<-max_sales[601:800,1]
# maxs_grp5<-max_sales[801:1000,1]


#Cluster on Store Type and Assortment
attach(GrandMasterData)
str(StoreType)
str(Assortment)

stA_asrtA<-as.integer(unique(GrandMasterData$Store[StoreType =="a" & Assortment=="a"]))
stA_asrtB<-as.integer(unique(GrandMasterData$Store[StoreType =="a" & Assortment=="b"]))
stA_asrtC<-as.integer(unique(GrandMasterData$Store[StoreType =="a" & Assortment=="c"]))

stB_asrtA<-as.integer(unique(GrandMasterData$Store[StoreType =="b" & Assortment=="a"]))     
stB_asrtB<-as.integer(unique(GrandMasterData$Store[StoreType =="b" & Assortment=="b"]))  
stB_asrtC<-as.integer(unique(GrandMasterData$Store[StoreType =="b" & Assortment=="c"]))  

stC_asrtA<-as.integer(unique(GrandMasterData$Store[StoreType =="c" & Assortment=="a"]))
stC_asrtB<-as.integer(unique(GrandMasterData$Store[StoreType =="c" & Assortment=="b"]))
stC_asrtC<-as.integer(unique(GrandMasterData$Store[StoreType =="c" & Assortment=="c"]))

stD_asrtA<-as.integer(unique(GrandMasterData$Store[StoreType =="d" & Assortment=="a"]))
stD_asrtB<-as.integer(unique(GrandMasterData$Store[StoreType =="d" & Assortment=="b"]))
stD_asrtC<-as.integer(unique(GrandMasterData$Store[StoreType =="d" & Assortment=="c"]))




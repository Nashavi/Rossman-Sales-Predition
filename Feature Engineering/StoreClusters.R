load("Datasets/GrandMasterData.RData")

head(GrandMasterData)

#Cluster on avg daily sales
avg_daily_sales<-as.data.frame(GrandMasterData %>% group_by(Store) %>% summarise(avgdailysales = mean(Sales)) %>% arrange(avgdailysales))
avgs_grp1<-avg_daily_sales[1:200,1]
avgs_grp2<-avg_daily_sales[201:400,1]
avgs_grp3<-avg_daily_sales[401:600,1]
avgs_grp4<-avg_daily_sales[601:800,1]
avgs_grp5<-avg_daily_sales[801:1000,1]



#Cluster on SD of store sales
var_sales<-as.data.frame(GrandMasterData %>% group_by(Store) %>% summarise(varsales = var(Sales)) %>% arrange(varsales))
vars_grp1<-var_sales[1:200,1]
vars_grp2<-var_sales[201:400,1]
vars_grp3<-var_sales[401:600,1]
vars_grp4<-var_sales[601:800,1]
vars_grp5<-var_sales[801:1000,1]


#Cluster on Max of store sales
max_sales<-as.data.frame(GrandMasterData %>% group_by(Store) %>% summarise(maxsales = max(Sales)) %>% arrange(maxsales))
maxs_grp1<-max_sales[1:200,1]
maxs_grp2<-max_sales[201:400,1]
maxs_grp3<-max_sales[401:600,1]
maxs_grp4<-max_sales[601:800,1]
maxs_grp5<-max_sales[801:1000,1]


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




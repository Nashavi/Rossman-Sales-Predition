
#load these datasets, lag the sales by 7,8,9,10 weeks. 
#save as new features
# save the datasets back down 

load("Datasets/TreeTraining.RData")
load("Datasets/TreeTesting.RData")
load("Datasets/TreeEval.RData")

d <- training

#update this for a given lag
weeks <- 6 
lag <- weeks * 7 

#chang new variable name
dlagged <- slide(d, Var = "Sales", GroupVar = "Store", NewVar= "SixWeekSalesLag",slideBy = -lag)

training <- dlagged

save(training,file="Datasets/TreeTraining.RData",compress = TRUE)


##### For TEst Dataset

# lag.stores <- unique(d[is.na(d$SixWeekSalesLag),"Store"])
# 
# for(i in 1:length(lag.stores)){
#   avg.sales <- mean(d[d$Store==i & d$year==2013 & d$month==1,"Sales"])
#   d[d$Store==i & is.na(d$SixWeekSalesLag)==TRUE,"SixWeekSalesLag"] <- avg.sales
#   print(paste(i,avg.sales))
# }
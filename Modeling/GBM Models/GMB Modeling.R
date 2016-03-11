##### Gradient Boosting Model ####

# Load Default Setup 
source("Modeling/Modeling Setup.R")


d <- training[training$State.ST==1,]


d<- d[,-which(names(d) %in% c("State.BBMVTH","State.NWRP","State.BEHBHHNISH","State.SN"))]
d<- d[,-which(names(d) %in% c("StateBW","State.BY","State.HE","State.ST"))]


load("Modeling/GBM Models/ST_gbmModel.RData")

plot(gbmFit)
r = which.min(gbmFit$results[,"RMSE"])#which combo had the best RMSE?
ntrees = gbmFit$results[r,"n.trees"] #what was the # of trees?
depth = gbmFit$results[r,"interaction.depth"] #how deep were the trees?
shrink = gbmFit$results[r,"shrinkage"] #what was the shrinkage?


gbmFit_new <- gbm(Sales~.,
                  data = d,
                  distribution = "gaussian",
                  n.trees = 5000,
                  interaction.depth = 3,
                  n.minobsinnode = 20,
                  shrinkage = 0.1,
                  cv.folds=3,
                  keep.data = TRUE,
                  verbose = "TRUE")








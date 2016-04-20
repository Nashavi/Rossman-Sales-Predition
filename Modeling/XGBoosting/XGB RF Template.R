source("Modeling/XGBoosting/XGB Setup.R")
class(deval)
class(dtrain)
class(Fulldtrain)
class(dtest)

tune.grid <- expand.grid(depth = c(5,10,15,20),
                         shrinkage = c(.5,.1,.05,.01),
                         ntrees = 5000,
                         EarlyStop = -1,
                         RMSE = rep(-1))

for(i in 1:length(tune.grid$depth)) {
  
  d <- tune.grid[i,"depth"]
  s <- tune.grid[i,"shrinkage"]
  t <- tune.grid[i,"ntrees"]
  
  
  bst <- xgb.train(data = dtrain
                 , max.depth = 4
                 , num_parallel_tree = 1000
                 , subsample = 0.5
                 , colsample_bytree = 0.5
                 , nround = 100
                 , watchlist= eval_watchlist
                 , early.stop.round = 5
                 , maximize = FALSE
                 , objective = "reg:linear")
  
save(bst,file="Modeling/XGBoosting/XGB RF Model 4 19.RData")  
  
  
  bst <- xgb.train(data=dtrain
                 , max.depth= d
                 , eta= s
                 , nthread = 2
                 , nround= t
                 , watchlist= eval_watchlist
                 , early.stop.round = 5
                 , maximize = FALSE
                 , objective = "reg:linear")

  ind <- bst$bestInd
  tune.grid[i,"EarlyStop"] <- ind
  e <- bst$bestScore
  tune.grid[i,"RMSE"] <- e
  
}

m <- which.min(tune.grid$RMSE)
d <- tune.grid[m,"depth"]
s <- tune.grid[m,"shrinkage"]
t <- tune.grid[m,"EarlyStop"]


#### now tune the entire training set using the best combo of paramters from 
#### tuning. Use the early stop value for nround to prevent overfitting. 

bst.best <- xgboost(data=Fulldtrain
                    , max.depth = 4
                    , num_parallel_tree = 1000
                    , subsample = 0.5
                    , colsample_bytree = 0.5
                    , nround = 100
                    #, watchlist= eval_watchlist
                    #, early.stop.round = 5
                    #, maximize = FALSE
                    , objective = "reg:linear")


pred <- predict(bst.best,dtest)

length(pred)
pred

load("Datasets/TestData.RData")
TestData <- TestData[,c(1:8)]
Pred <- cbind(TestData,pred)
Pred$Sales <- round(exp(Pred$pred),2)
Pred$Sales <- ifelse(Pred$Open==0,0,Pred$Sales)
Pred[Pred$Id==907,]

write.csv(Pred,file="Prediction Files/April 20 XGB RF Preds.csv")


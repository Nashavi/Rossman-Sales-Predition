source("Modeling/XGBoosting/XGB Setup.R")
class(deval)
class(dtrain)
class(Fulldtrain)
class(dtest)

tune.grid <- expand.grid(depth = c(15),
                         shrinkage = c(.5,.1,.05),
                         ntrees = 1000,
                         RMSE = rep(-1))

for(i in 1:length(tune.grid$depth)) {
  
  d <- tune.grid[i,"depth"]
  s <- tune.grid[i,"shrinkage"]
  t <- tune.grid[i,"ntrees"]
  
  bst <- xgb.train(data=dtrain
                 , max.depth=d#15
                 , eta=s#.05
                 , nthread = 2
                 , nround=t#1000
                 , watchlist=eval_watchlist
                 , early.stop.round = 5
                 , maximize = FALSE
                 , objective = "reg:linear")

  e <- bst$bestScore
  tune.grid[i,"RMSE"] <- e
  
}

tune.grid


#### now tune the entire training set using the best combo of paramters from 
#### tuning. Use the early stop value for nround to prevent overfitting. 

bst.best <- xgboost(data=Fulldtrain
                 , max.depth=15
                 , eta=.05
                 , nthread = 2
                 , nround=192
                 #, watchlist=eval_watchlist
                 #, early.stop.round = 5
                 #, maximize = FALSE
                 , objective = "reg:linear")


pred <- predict(bst,dtest)

length(pred)
pred

load("Datasets/TestData.RData")
TestData <- TestData[,c(1:8)]
Pred <- cbind(TestData,pred)
Pred$Sales <- round(exp(Pred$pred),2)
Pred$Sales <- ifelse(Pred$Open==0,0,Pred$Sales)
Pred[Pred$Id==907,]

write.csv(Pred,file="Prediction Files/April 17 XGB Preds.csv")


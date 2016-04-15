source("Modeling/XGBoosting/XGB Setup.R")
class(deval)
class(dtrain)



bst <- xgb.train(data=dtrain
                 , max.depth=6
                 , eta=.1
                 , nthread = 2
                 , nround=50
                 , watchlist=eval_watchlist
                 , objective = "reg:linear")
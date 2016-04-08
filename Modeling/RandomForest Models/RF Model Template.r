
# Load Default Setup 
source("Modeling/Tree Modeling Setup.R")

rm(StoreGroups)                           #already assigned this variable as a new feature. 

##### dataframe subsettting and prep ##### 
#Training
d <- training
rm(training)
d <- d[d$Open==1 & d$Sales!=0,]           # remove $0 sales 
d <- d[,-c(1:4)]                          # remove X, Date, Open, and Store ID 
Sales <- log(d$Sales)                     # now change sales to log sales and save to a vector 
Vars <- d[,-3]                            # now save a new dataframe without the response
rm(d)                                     # remove d

#Testing
ts <- testing
rm(testing)
ts <- ts[ts$Open==1 & ts$Sales!=0,]       # remove $0 sales 
ts <- ts[,-c(1:4)]                        # remove X, Date, Open, and Store ID 
ts.Sales <- log(ts$Sales)                 # now change sales to log sales and save to a vector 
ts.Vars <- ts[,-3]                        # now save a new dataframe without the response
rm(ts)                                    # remove d

#Eval
e <- eval
rm(eval)
e <- e[e$Open==1 & e$Sales!=0,]           # remove $0 sales 
e <- e[,-c(1:4)]                          # remove X, Date, Open, and Store ID 
e.Sales <- log(e$Sales)                   # now change sales to log sales and save to a vector 
e.Vars <- e[,-3]                          # now save a new dataframe without the response
rm(e)                                    # remove d

################### MODEL TUNING ################## 

p = dim(Vars)[2]                        # how many variables are available?
m <- round(p/3)                         # mytry should be ~ p/3
n <- 15                                 # how much to bracket mtry for training
t <- 5                                  # how many mtry values do you want to try?
lbound <- m - n                         # lower bound of mtry values
ubound <- m + n                         # upper bound of mtry values 
nTrees <- 100                           # how many trees should be built?

#Tune Grid 
tune.grid <- expand.grid(mtry = seq(lbound,ubound,by=t),
                         ntrees = nTrees,
                         RMSE1 = rep(-1),
                         RMSE2 = rep(-1))

# RF Model Hyper Paramter Training Loop
for(i in 1:length(tune.grid$mtry)) {
  
  mtrys <- tune.grid[i,"mtry"]
  ntrees <- tune.grid[i,"ntrees"]
  
  print(paste("Model",i,":",ntrees, "trees,",mtrys,"mtry."))
  
  rfFit <- randomForest(Vars,
                      Sales,
                      mtry=mtrys,
                      ntree=ntrees,
                      do.trace = TRUE)
  
  plot(rfFit)
  summary(rfFit)[1:10,]
  
  # apply model to test set:
  test.preds <- predict(rfFit,newdata = ts.Vars)
  tune.grid[i,"RMSE1"] <- sqrt(mean((ts.Sales-test.preds)^2))
  print(paste("model",i,"Test RMSE:",sqrt(mean((ts.Sales-test.preds)^2))))
  
  
  eval.preds <- predict(rfFit,newdata = e.Vars)
  tune.grid[i,"RMSE2"] <- sqrt(mean((e.Sales-eval.preds)^2))
  print(paste("model",i,"Eval RMSE:",sqrt(mean((e.Sales-eval.preds)^2))))

  print(paste("Model",i,"Done."))
  
}














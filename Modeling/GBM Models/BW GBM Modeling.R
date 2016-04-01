
# Load Default Setup 
source("Modeling/Modeling Setup.R")

st <- "BW"
training <- training[training$Open==1 & training$Sales!=0 & training$State==st, ]
training$Sales <- log(training$Sales)
training <- training[,-c(1,3:5)]

testing <- testing[testing$Open==1 & testing$Sales!=0 & testing$State==st, ]
testing$Sales <- log(testing$Sales)
testing <- testing[,-c(1,3:5)]

######### Modeling ########

tune.grid <- expand.grid(ntrees = seq(3000,4000,by=200),
                         depth = c(4,5),
                         shrink = c(0.1),
                         RMSE = rep(-1) )

for(i in 1:length(tune.grid$ntrees)) {
 
  ntrees <- tune.grid[i,"ntrees"]
  depth <- tune.grid[i,"depth"]
  shrink <- tune.grid[i,"shrink"]

  print(paste("model#",i," - trying",ntrees,"trees,",depth,"deep with",shrink,"shrinkage"))
  
  gbmMod <- gbm(Sales ~ .
              , data = training
              ,n.trees=ntrees
              ,interaction.depth =depth
              ,shrinkage=shrink
              ,distribution = "gaussian")

  print(paste("model",i,"tuned, now applying preds..."))

  gbmPreds <- predict(gbmMod
                    ,testing
                    ,n.trees = ntrees
                    ,interaction.depth = depth
                    ,shrinkage = shink
                    ,type="response")
    
  tune.grid[i,"RMSE"] <- sqrt(mean((gbmPreds - testing$Sales)^2))
  
  print(sqrt(mean((gbmPreds - testing$Sales)^2)))
  print(paste("model",i,"finished."))
  
}
tune.grid <- data.frame(tune.grid)
attach(tune.grid)
ggplot(tune.grid,aes(ntrees,RMSE))+geom_line(lwd=1,aes(color=factor(depth)))+facet_wrap(~depth)+ggtitle("Boosting Results")
tune.gridmin <- which.min(tune.grid$RMSE)
tune.grid[tune.gridmin,]

##### BW Results 
#1) 3000 trees, 5 deep, .1 shrink = 12.63% RMSE
#2) 4000 trees, 4 deep, .1 shink = 12.87% RMSE 
#3) 3800 trees, 5 deep, .1 shrink = 12.51% RMSE
########





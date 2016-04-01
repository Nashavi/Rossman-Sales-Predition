
# Load Default Setup 
source("Modeling/Modeling Setup.R")

st <- "BY"
training <- training[training$Open==1 & training$Sales!=0 & training$State==st, ]
training$Sales <- log(training$Sales)
training <- training[,-c(1,3:5)]

testing <- testing[testing$Open==1 & testing$Sales!=0 & testing$State==st, ]
testing$Sales <- log(testing$Sales)
testing <- testing[,-c(1,3:5)]

######### Modeling ########

tune.grid <- expand.grid(ntrees = seq(3000,4000,by=250),
                         depth = c(5,6),
                         shrink = c(0.1),
                         RMSE = rep(-1) )

for(i in 1:length(tune.grid$ntrees)) {
 
  ntrees <- tune.grid[i,"ntrees"]
  depth <- tune.grid[i,"depth"]
  shrink <- tune.grid[i,"shrink"]

  print(paste("ntrees:",ntrees,"depth:",depth,"shrink:",shrink))
  
  gbmMod <- gbm(Sales ~ .
              , data = training
              ,n.trees=ntrees
              ,interaction.depth =depth
              ,shrinkage=shrink
              ,distribution = "gaussian")

  print(paste(i,"th model tuned, now applying preds..."))

  gbmPreds <- predict(gbmMod
                    ,testing
                    ,n.trees = ntrees
                    ,interaction.depth = depth
                    ,shrinkage = shink
                    ,type="response")
    
  tune.grid[i,"RMSE"] <- sqrt(mean((gbmPreds - testing$Sales)^2))
  
  print(sqrt(mean((gbmPreds - testing$Sales)^2)))
  print(paste(i,"th model finished."))
  
}
tune.grid <- data.frame(tune.grid)
attach(tune.grid)
ggplot(tune.grid,aes(ntrees,RMSE))+geom_line(lwd=1,aes(color=factor(depth)))+facet_wrap(~depth)+ggtitle("Boosting Results")
tune.gridmin <- which.min(tune.grid$RMSE)
tune.grid[min,]

##### Results 
#1) 2000 trees, 3 deep, .1 shrink = 16.55% RMSE
#2) 3000 trees, 5 deep, .1 shink = 14.73% RMSE   (at 3000 trees 4 deep, RMSE seems to be coming back up...)
#3)
########





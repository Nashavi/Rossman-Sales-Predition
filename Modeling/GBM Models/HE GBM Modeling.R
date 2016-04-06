
# Load Default Setup 
source("Modeling/Tree Modeling Setup.R")

unique(training$State)

st <- "HE"
training <- training[training$Open==1 & training$Sales!=0 & training$State==st, ]
training$Sales <- log(training$Sales)
training <- training[,-c(1:4)]

testing <- testing[testing$Open==1 & testing$Sales!=0 & testing$State==st, ]
testing$Sales <- log(testing$Sales)
testing <- testing[,-c(1:4)]

eval <- eval[eval$Open==1 & eval$Sales!=0 & eval$State==st, ]
eval$Sales <- log(eval$Sales)
eval <- eval[,-c(1:4)]

######### Modeling ########

tune.grid <- expand.grid(ntrees = seq(3000,5000,by=500),
                         depth = c(4,5,6),
                         shrink = c(0.1),
                         RMSE1 = rep(-1),
                         RMSE2 = rep(-1))

for(i in 1:length(tune.grid$ntrees)) {
 
  ntrees <- tune.grid[i,"ntrees"]
  depth <- tune.grid[i,"depth"]
  shrink <- tune.grid[i,"shrink"]

  print(paste("model",i,":",ntrees, "trees,",depth,"deep,",shrink,"learning rate."))
  
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
    
  tune.grid[i,"RMSE1"] <- sqrt(mean((gbmPreds - testing$Sales)^2))
  
  print(sqrt(mean((gbmPreds - testing$Sales)^2)))
  
  gbmPreds <- predict(gbmMod
                      ,eval
                      ,n.trees = ntrees
                      ,interaction.depth = depth
                      ,shrinkage = shink
                      ,type="response")
  
  tune.grid[i,"RMSE2"] <- sqrt(mean((gbmPreds - eval$Sales)^2))
  
  print(sqrt(mean((gbmPreds - eval$Sales)^2)))
  
  print(paste("model",i,"finished."))
  
}
tune.grid <- data.frame(tune.grid)
attach(tune.grid)
ggplot(tune.grid,aes(ntrees,RMSE1))+geom_line(lwd=1,aes(color=factor(depth)))+facet_wrap(~depth)+ggtitle("Boosting Results")
tune.gridmin <- which.min(tune.grid$RMSE1)
tune.grid[tune.gridmin,]

ggplot(tune.grid,aes(ntrees,RMSE2))+geom_line(lwd=1,aes(color=factor(depth)))+facet_wrap(~depth)+ggtitle("Boosting Results")
tune.gridmin <- which.min(tune.grid$RMSE2)
tune.grid[tune.gridmin,]

##### Results 
#1) 
#2) 
#3) 
########






source("Modeling/XGBoosting/Boosting Setup.R")
rm(StoreGroups)

# training data setup 
training <- training[training$Open==1 & training$Sales!=0,]
train_response = log(training$Sales)
training <-training[,-c(1:4)]
training <-training[,-3]
df <- data.table(training,keep.rownames = F)
rm(training)
train_matrix <- sparse.model.matrix(~., data = df)



#eval data setup
eval <- rbind(testing,eval)
rm(testing)
eval <- eval[eval$Open==1 & eval$Sales!=0,]
eval_response = log(eval$Sales)
eval <-eval[,-c(1:4)]
eval <-eval[,-3]
df <- data.table(eval,keep.rownames = F)
rm(eval)
eval_matrix <- sparse.model.matrix(~., data = df)


rm(df)

tune.grid <- expand.grid(depth = c(3,4,5,6)#seq(lbound,ubound,by=t),
                         ,trees = seq(500,1000,by=250)#nTrees,
                         ,shrinkage = c(.1)
                         ,RMSE = rep(-1))

for(i in 1:length(tune.grid$depth)) {

  d <- tune.grid[i,"depth"]
  n <- tune.grid[i,"trees"]
  e <- tune.grid[i,"shrinkage"]
  
  print(paste(i,d,n,e))
  
  bst <- xgboost(data = train_matrix
                 , label = train_response
                 , max.depth = d
                 , eta = e
                 , nthread = 2
                 , nround = n
                 , objective = "reg:linear"
              )

  pred <- predict(bst, eval_matrix)

  tune.grid[i,"RMSE"] <- sqrt(mean((eval_response-pred)^2))
  print(paste("model",i,"Test RMSE:",sqrt(mean((eval_response-pred)^2))))
  
  print(paste("Model",i,"Done."))
  
}
ggplot(tune.grid,aes(trees,RMSE))+geom_line(lwd=1,aes(color=factor(depth)))+facet_wrap(~depth)+ggtitle("XGB Results")

#what was the best set of hyper parameters?
min <- which.min(tune.grid$RMSE)
d <- tune.grid[min,"depth"] # 6
n <- tune.grid[min,"trees"] # 500
e <- tune.grid[min,"shrinkage"] #.1

# now reload the data and bind the entire training set into a single matrix
source("Modeling/XGBoosting/Boosting Setup.R")

rm(StoreGroups)

# final training data setup 
training <- rbind(training,testing)
training <- rbind(training,eval)
training <- training[training$Open==1 & training$Sales!=0,]
train_response = log(training$Sales)
training <-training[,-c(1:4)]
training <-training[,-3]
df <- data.table(training,keep.rownames = F)
rm(training)
rm(testing)
rm(eval)
train_matrix <- sparse.model.matrix(~., data = df)

# final training using the best hyper parameters
bst.best <- xgboost(data = train_matrix
               , label = train_response
               , max.depth = 6
               , eta = .1
               , nthread = 2
               , nround = 500
               , objective = "reg:linear")

save(bst.best,file="Modeling/XGBoosting/XGB 6 deep 500 Trees .1 Learning.RData")

load("Datasets/TreeFinalTestData.RData")
#Test Setup
t <- TreeFinalTestData
t$Open.0 <- ifelse(t$Open==0,1,0)
t$Open.1 <- ifelse(t$Open==1,1,0)
t$Open._L11 <- ifelse(t$Open_L1.1==1,1,0)
t$Open._L12 <- ifelse(t$Open_L1.2==1,1,0)
t$Open._L13 <- ifelse(t$Open_L1.3==1,1,0)
t <- t[,-c(1:4)]

df <- data.table(t,keep.rownames = F)
rm(t)
test_matrix <- sparse.model.matrix(~., data = df)
rm(df)
pred <- predict(bst.best, test_matrix)

load("Modeling/finalPreds.RData")
finalPreds$Sales <- 0
finalPreds$Sales <- exp(pred)
finalPreds$Sales <- ifelse(finalPreds$Open==0,0,finalPreds$Sales)

save(finalPreds,file="Modeling/XGBoosting/FinalPreds.RData")


write.csv(finalPreds,file="Prediction Files/April 12 Preds.csv")



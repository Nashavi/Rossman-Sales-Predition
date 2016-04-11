
source("Modeling/XGBoosting/Boosting Setup.R")
rm(StoreGroups)

# training data setup 
training <- training[training$Open==1 & training$Sales!=0,]
training <-training[,-c(1:4)]
training$Sales <- log(training$Sales)
df <- data.table(training,keep.rownames = F)
rm(training)
train_matrix <- sparse.model.matrix(Sales~.-1, data = df)
train_response = df[,Sales] 


#eval data setup
eval <- rbind(testing,eval)
rm(testing)
eval <- eval[eval$Open==1 & eval$Sales!=0,]
eval <-eval[,-c(1:4)]
eval$Sales <- log(eval$Sales)
df <- data.table(eval,keep.rownames = F)
rm(eval)
eval_matrix <- sparse.model.matrix(Sales~.-1, data = df)
eval_response = df[,Sales] 

rm(df)

tune.grid <- expand.grid(depth = c(2,3,4,5),#seq(lbound,ubound,by=t),
                         nround = c(50,100,150,200),#nTrees,
                         RMSE = rep(-1))

for(i in 1:length(tune.grid$depth)) {

  d <- tune.grid[i,"depth"]
  n <- tune.grid[i,"nround"]
  
  bst <- xgboost(data = train_matrix
                 , label = train_response
                 , max.depth = d
                 , eta = 1
                 , nthread = 2
                 , nround = n
                 , objective = "reg:linear")

  pred <- predict(bst, eval_matrix)

  tune.grid[i,"RMSE"] <- sqrt(mean((eval_response-pred)^2))
  print(paste("model",i,"Test RMSE:",sqrt(mean((eval_response-pred)^2))))
  
  print(paste("Model",i,"Done."))
  
}
ggplot(tune.grid,aes(nround,RMSE))+geom_line(lwd=1,aes(color=factor(depth)))+facet_wrap(~depth)+ggtitle("Boosting Results")

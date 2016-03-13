##### Gradient Boosting Model ####

# Load Default Setup 
source("Modeling/Modeling Setup.R")

states <- unique(training$State)
states

st <- "BEHBHHNISH" 

d <- training[training$State==st & training$Open==1 & training$Sales!=0,]
d$Sales <- log(d$Sales)
d <- d[,-which(names(d) %in% c("Open","State","Store"))]
rm(training)

ts <- testing[testing$State==st & testing$Open==1 & testing$Sales!=0,]
ts$Sales <- log(ts$Sales)
ts <- ts[,-which(names(ts) %in% c("Open","State","Store"))]
rm(testing)

e <- eval[eval$State==st & eval$Open==1 & eval$Sales!=0,]
e$Sales <- log(e$Sales)
e <- e[,-which(names(e) %in% c("Open","State","Store"))]
rm(eval)

fitControl = trainControl(method = 'cv'
                          , number=3
                          ,summaryFunction=defaultSummary)
gbmGrid =  expand.grid(interaction.depth= seq(2,3,by=1), #which tree depth values to try
                       n.trees = seq(1000,5000,by=500), #how many values of n.trees to try
                       shrinkage = c(0.1),
                       n.minobsinnode=20)
gbmFit = train(Sales ~ .
               ,data = d
               ,method='gbm'
               ,trControl=fitControl
               ,metric="RMSE"
               ,tuneGrid=gbmGrid
               ,verbose=TRUE) 

save(gbmFit,file="Modeling/GBM Models/BEHBHHNISH_GBMModel.RData",compress = TRUE)

summary(gbmFit)[1:10,]
gbmFit$bestTune #what was the best tune?
plot(gbmFit)
gbmFit$results
r = which.min(gbmFit$results[,"RMSE"])#which combo had the best RMSE?
ntrees = gbmFit$results[r,"n.trees"] #what was the # of trees?
depth = gbmFit$results[r,"interaction.depth"] #how deep were the trees?
shrink = gbmFit$results[r,"shrinkage"] #what was the shrinkage?

test.preds <- predict(gbmFit,newdata = ts)
eval.preds <- predict(gbmFit,newdata = e)

sqrt(mean((ts$Sales-test.preds)^2))
sqrt(mean((e$Sales-eval.preds)^2))

#save(gbmFit,file="Modeling/GBM Models/TestGBMModel.RData",compress = TRUE)

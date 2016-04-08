# Load Default Setup 
source("Modeling/Modeling Setup.R")
require(elasticnet)
library(glmnet)

unique(training$State)
st <- "NWRP"

training <- training[training$Open==1 & training$Sales!=0 & training$State==st, ]
training <- training[,-c(1,3:5)]
x = data.frame(nearZeroVar(training, saveMetrics = TRUE))
zerovarcols <- which(x$zeroVar==TRUE)
training <- training[,-zerovarcols]
training.Sales <- log(training$Sales)
training <- model.matrix(Sales~.,training)[,-1]


testing <- testing[testing$Open==1 & testing$Sales!=0 & testing$State==st, ]
testing <- testing[,-c(1,3:5)]
testing <- testing[,-zerovarcols]
testing.Sales <- log(testing$Sales)
testing <- model.matrix(Sales~.,testing)[,-1]

eval <- eval[eval$Open==1 & eval$Sales!=0 & eval$State==st, ]
eval <- eval[,-c(1,3:5)]
eval <- eval[,-zerovarcols]
eval.Sales <- log(eval$Sales)
eval <- model.matrix(Sales~.,eval)[,-1]


grid <- 10^seq(10,-2,length=1000)
lasso.mod <- glmnet(training,training.Sales,alpha=0)#,lambda=grid) #alpha = 1 === Lasso Model 
n <- dim(coef(lasso.mod))[2] # how many lambda values are there?

for(i in 1:n){
  lam <- lasso.mod$lambda[i]
  lasso.preds <- predict(lasso.mod
                        ,s = lam 
                        ,newx = testing)
  
  rmse <- sqrt(mean((lasso.preds - testing.Sales)^2))
  print(rmse)
}


enetModel <- enet(x = training,
                  y = training.Sales,
                  lambda = .01,
                  normalize = TRUE)

enetPreds <- predict(enetModel
                     ,testing
                     ,s = .01
                     ,mode = "fraction"
                     ,type = "fit")

#enetGrid[i,"RMSE1"] <- sqrt(mean((enetPreds$fit - testing.Sales)^2))

print(sqrt(mean((enetPreds$fit - testing.Sales)^2)))


enetGrid <- expand.grid(lambda = seq(1,10,by=1),
                        RMSE1 = rep(-1),
                        RMSE2 = rep(-1))

for(i in 1:length(enetGrid$lambda)) {
<<<<<<< HEAD
  
  .l <- enetGrid[i,1]
 
  print(paste(i,"model:","lambda=",.l))
=======
 
  .lambda <- enetGrid[i,1]
 
  print(paste(i,"model:","lambda=",.lambda,"Start Time:",Sys.time()))
>>>>>>> c4224a82b8b0a0bf709f53a04e74d9f04344f927
  
  enetModel <- enet(x = training,
                    y = training.Sales,
                    lambda = .l,
                    normalize = TRUE)
  
  print(paste(i,"th model tuned, now applying preds..."))
  
  enetPreds <- predict(enetModel
                      ,testing
                      ,s = .1
                      ,mode = "fraction"
                      ,type = "fit")
  
  enetGrid[i,"RMSE1"] <- sqrt(mean((enetPreds$fit - testing.Sales)^2))
  
  print(sqrt(mean((enetPreds$fit - testing.Sales)^2)))
  
  enetPreds <- predict(enetModel
                       ,eval
                       ,s = .1
                       ,mode = "fraction"
                       ,type = "fit")
  
  enetGrid[i,"RMSE2"] <- sqrt(mean((enetPreds$fit - eval.Sales)^2))
  
  print(sqrt(mean((enetPreds$fit - eval.Sales)^2)))
  
  print(paste(i,"th model finished.","End time",Sys.time()))
  
}
enetGrid <- data.frame(enetGrid)
attach(enetGrid)
ggplot(enetGrid,aes(lambda,RMSE1))+geom_line()#lwd=1,aes(color=factor("depth")))+facet_wrap(~depth)+ggtitle("Boosting Results")
enetGridmin <- which.min(enetGrid$RMSE1)
enetGrid[enetGridmin,]

ggplot(enetGrid,aes(lambda,RMSE2))+geom_line()#lwd=1,aes(color=factor(depth)))+facet_wrap(~depth)+ggtitle("Boosting Results")
enetGridmin <- which.min(enetGrid$RMSE2)
enetGrid[enetGridmin,]


# 
# enetModel <- enet(training,
#      training.Sales,
#      .1,
#      normalize=TRUE)
# 
# enetPreds <- predict(enetModel
#                      ,testing
#                      ,s = .1
#                      ,mode = "fraction"
#                      ,type = "fit")
# 
# sqrt(mean((enetPreds$fit - testing.Sales)^2))



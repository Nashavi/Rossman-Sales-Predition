
# Load Default Setup 
source("Modeling/Modeling Setup.R")
require(elasticnet)

st <- "BW"

training <- training[training$Open==1 & training$Sales!=0 & training$State==st, ]
training.Sales <- log(training$Sales)
training <- training[,-c(1:5)]
x = data.frame(nearZeroVar(training, saveMetrics = TRUE))
zerovarcols <- which(x$zeroVar==TRUE)
training <- training[,-zerovarcols]
training <- as.matrix(training)

testing <- testing[testing$Open==1 & testing$Sales!=0 & testing$State==st, ]
testing.Sales <- log(testing$Sales)
testing <- testing[,-c(1:5)]
testing <- as.matrix(testing)

eval <- eval[eval$Open==1 & eval$Sales!=0 & eval$State==st, ]
eval.Sales <- log(eval$Sales)
eval <- eval[,-c(1:5)]
eval <- as.matrix(eval)


enetGrid <- expand.grid(lambda = seq(0,.1,by=.01),
                        RMSE1 = rep(-1),
                        RMSE2 = rep(-1))

for(i in 1:length(enetGrid$lambda)) {
  
  .lambda <- enetGrid[i,1]
 
  print(paste(i,"model:","lambda=",.lambda))
  
  enetModel <- enet(training,
                    training.Sales,
                    .lambda,
                    normalize=TRUE)
  
  print(paste(i,"th model tuned, now applying preds..."))
  
  enetPreds <- predict(enetModel
                      ,newX = testing
                      ,s = .1
                      ,mode = "fraction"
                      ,type = "fit")
  
  enetGrid[i,"RMSE1"] <- sqrt(mean((enetPreds$fit - testing.Sales)^2))
  
  print(sqrt(mean((enetPreds$fit - testing.Sales)^2)))
  
  enetPreds <- predict(enetModel
                       ,newX = eval
                       ,s = .1
                       ,mode = "fraction"
                       ,type = "fit")
  
  enetGrid[i,"RMSE2"] <- sqrt(mean((gbmPreds$fit - eval.Sales)^2))
  
  print(sqrt(mean((gbmPreds$fit - eval.Sales)^2)))
  
  print(paste(i,"th model finished."))
  
}
enetGrid <- data.frame(enetGrid)
attach(enetGrid)
ggplot(enetGrid,aes(lambda,RMSE1))+geom_line()#lwd=1,aes(color=factor("depth")))+facet_wrap(~depth)+ggtitle("Boosting Results")
enetGridmin <- which.min(enetGrid$RMSE1)
enetGrid[enetGridmin,]

ggplot(enetGrid,aes(lambda,RMSE2))+geom_line()#lwd=1,aes(color=factor(depth)))+facet_wrap(~depth)+ggtitle("Boosting Results")
enetGridmin <- which.min(enetGrid$RMSE2)
enetGrid[enetGridmin,]


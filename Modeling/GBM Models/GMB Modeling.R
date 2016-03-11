##### Gradient Boosting Model ####

# Load Default Setup 
source("Modeling/Modeling Setup.R")

st <- "ST" 

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
                          ,summaryFunction=summaryFunction)
gbmGrid =  expand.grid(interaction.depth= 3,#seq(2,3,by=1), #which tree depth values to try
                       n.trees = 5000, #seq(4000,5000,by=200), #how many values of n.trees to try
                       shrinkage = c(0.1),
                       n.minobsinnode=20)
gbmFit = train(Sales ~ .
               ,data = training
               ,method='gbm'
               ,trControl=fitControl
               ,metric="RMSE"
               ,tuneGrid=gbmGrid
               ,verbose=TRUE) 





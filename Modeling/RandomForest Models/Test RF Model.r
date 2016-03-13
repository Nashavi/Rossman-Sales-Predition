

# Load Default Setup 
source("Modeling/Modeling Setup.R")

d <- training
rm(training)

##### training subsettting and prep ##### 
d <- d[,-1] #remove index
d <- d[d$Open==1 & d$Sales!=0,] #subset open and >0 sales
d <- d[,-2] #remove open 
d <- d[,-3] #remove stores 

unique(d$State)

d <- d[d$State=="SN",] #choose a store and subset
d <- d[,-2] #now remove state

d$Sales <- log(d$Sales) #now change sales to log sales 



##### model hyperparamter eval ################## 

fitControl = trainControl(method = 'cv'
                          , number=3
                          ,summaryFunction=defaultSummary)
Grid =  expand.grid(mtry=10)#c(90,100,110,120,130,140,150))
rfFit = train(Sales ~ .
              ,data = d
              ,method='rf'
              ,trControl=fitControl
              ,metric="RMSE"
              ,tuneGrid=Grid
              ,verbose=TRUE)






load("Datasets/NewFinalTest.RData")



t$Sales <- ifelse(t$Open==0,0,test.preds <- predict(gbmFit,newdata = t))

X <- t$X
Open <- t$Open
State <- t$State
Sales <- t$Sales

new <- cbind(Sales,State)
new <- cbind(Open,new)
new <- cbind(X,new)

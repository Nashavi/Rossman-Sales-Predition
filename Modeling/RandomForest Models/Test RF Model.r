
# Load Default Setup 
source("Modeling/Modeling Setup.R")

unique(training$State)
st <- "NWRP"

rm(testing) #remove to optimize memory 
rm(eval) #remove to optimize memory 

##### training subsettting and prep ##### 
d <- training
rm(training)
d <- d[,-1] #remove index
d <- d[d$Open==1 & d$Sales!=0,] #subset open and >0 sales
d <- d[,-2] #remove open 
d <- d[,-3] #remove stores 
d <- d[d$State==st,] #choose a state and subset
d <- d[,-2] #now remove state
d$Sales <- log(d$Sales) #now change sales to log sales 

################### MODEL TUNING ################## 

p = dim(d)[2]-1 #how many variables are available?
p <- round(p/3) #mytry should be ~ p/3
mtry.vec <- c(110,120,130) ### try p/3 bracketed up and down -30 tp + 30

fitControl = trainControl(method = 'cv'
                          , number=3
                          ,verboseIter = T
                          ,summaryFunction=defaultSummary)
Grid =  expand.grid(mtry=mtry.vec)
rfFit = train(Sales ~ .
              ,data = d
              ,method='rf'
              ,trControl=fitControl
              ,metric="RMSE"
              ,tuneGrid=Grid
              ,verbose=TRUE)

save(rfFit,file="Modeling/RandomForest Models/NWRP_RFModel.RData",compress = TRUE)

plot(rfFit)
summary(rfFit)[1:10,]

#### NOW LOAD TESTING AND EVAL TO SEE HOW GOOD RMSE IS #####

source("Modeling/Modeling Setup.R")
rm(d) #memory
rm(training) #memory 

ts <- testing
rm(testing)
ts <- ts[,-1] #remove index
ts <- ts[ts$Open==1 & ts$Sales!=0,] #subset open and >0 sales
ts <- ts[,-2] #remove open 
ts <- ts[,-3] #remove stores 
ts <- ts[ts$State==st,] #choose a state and subset
ts <- ts[,-2] #now remove state
ts$Sales <- log(ts$Sales) #now change sales to log sales 

e <- eval
rm(eval)
e <- e[,-1] #remove index
e <- e[e$Open==1 & e$Sales!=0,] #subset open and >0 sales
e <- e[,-2] #remove open 
e <- e[,-3] #remove stores 
e <- e[e$State==st,] #choose a state and subset
e <- e[,-2] #now remove state
e$Sales <- log(e$Sales) #now change sales to log sales 




test.preds <- predict(rfFit,newdata = ts)
eval.preds <- predict(rfFit,newdata = e)

sqrt(mean((ts$Sales-test.preds)^2))
sqrt(mean((e$Sales-eval.preds)^2))


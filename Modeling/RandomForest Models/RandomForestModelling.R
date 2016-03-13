##### Random Forest Model ####

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
                          , number=5
                          ,summaryFunction=defaultSummary)
Grid =  expand.grid(mtry=c(90,100,110,120,130,140,150))
rfFit = train(Sales ~ .
              ,data = d
              ,method='rf'
              ,trControl=fitControl
              ,metric="RMSE"
              ,tuneGrid=Grid
              ,verbose=TRUE)

plot(rfFit)

save(rfFit,file="Modeling/RandomForest Models/ST_RFModel.RData")



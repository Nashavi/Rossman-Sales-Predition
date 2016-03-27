
# Load Default Setup 
source("Modeling/Modeling Setup.R")

st <- "BW"

training <- training[training$Open==1 & training$Sales!=0 & training$State==st,]
Sales <- log(training$Sales)
training <- training[,-c(1:5)]

x = data.frame(nearZeroVar(training, saveMetrics = TRUE))
zerovarcols <- which(x$zeroVar==TRUE)

newTrain <- training[,-zerovarcols]

x = data.frame(nearZeroVar(newTrain, saveMetrics = TRUE))
zerovarcols <- which(x$zeroVar==TRUE)


training <- as.matrix(newTrain)
require(elasticnet)

enetGrid <- expand.grid(.lamba = c(0, 0.01, .1),
                        .fraction = seq(.5,1,length = 10))

ctrl <- trainControl(method="boot632")

enetTune <- train(x = training, y = Sales,
                  method = "enet",
                  tuneGrid = enetGrid, 
                  trControl = ctrl,
                  preProc = c("center","scale"))
  

enetModel <- enet(x = training, y = Sales, 
                  lambda = 0.01,normalize = TRUE)  
plot(enetModel)


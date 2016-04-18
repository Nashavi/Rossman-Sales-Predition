
#load required libraries 
library(caret)
# library(randomForest)
# library(gbm)
# library(ggplot2)
library(xgboost)
library(Matrix)
library(data.table)

#set the random seed
set.seed(686)

#load datasets for modeling
load("Datasets/TrainData.RData")
load("Datasets/EvalData.RData")
load("Datasets/FullTrainData.RData")
load("Datasets/TestData.RData")

#Train Set
TrainData <- TrainData[TrainData$Open==1 & TrainData$Sales!=0,]
Train_Sales = log(TrainData$Sales)
TrainData <- TrainData[,-c(1,3:5)] #remove id, store, date, sales 
TrainData <- data.table(TrainData,keep.rownames = F)
Train_Matrix <- sparse.model.matrix(~., data = TrainData)
dtrain <- xgb.DMatrix(data = Train_Matrix, label = Train_Sales)

# Eval Set
EvalData <- EvalData[EvalData$Open==1 & EvalData$Sales!=0,]
Eval_Sales = log(EvalData$Sales)
EvalData <- EvalData[,-c(1,3:5)] #remove id, store, date, sales 
EvalData <- data.table(EvalData,keep.rownames = F)
Eval_Matrix <- sparse.model.matrix(~., data = EvalData)
deval <- xgb.DMatrix(data = Eval_Matrix, label = Eval_Sales)

eval_watchlist <- list(eval=deval,train=dtrain)

#Full Training Set 
FullTrainData <- FullTrainData[FullTrainData$Open==1 & FullTrainData$Sales!=0,]
FullTrain_Sales = log(FullTrainData$Sales)
FullTrainData <- FullTrainData[,-c(1,3:5)] #remove id, store, date, sales 
FullTrainData <- data.table(FullTrainData,keep.rownames = F)
FullTrain_Matrix <- sparse.model.matrix(~., data = FullTrainData)
Fulldtrain <- xgb.DMatrix(data = FullTrain_Matrix, label = FullTrain_Sales)

#Test Set 
Test_Sales <- TestData$Sales
TestData <- TestData[,-c(1,3:5)] #remove store, date, sales 
TestData <- data.table(TestData,keep.rownames = F)
Test_Matrix <- sparse.model.matrix(~., data = TestData)
dtest <- xgb.DMatrix(data = Test_Matrix, label = Test_Sales)




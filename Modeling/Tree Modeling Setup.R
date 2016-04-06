#load required libraries 
library(caret)
library(randomForest)
library(gbm)
library(ggplot2)
#library(xgboost)

#set the random seed
set.seed(686)

#load datasets for modeling
load("Datasets/TreeTraining.RData")
load("Datasets/TreeTesting.RData")
load("Datasets/TreeEval.RData")
#load("Datasets/FinalTest.RData")



